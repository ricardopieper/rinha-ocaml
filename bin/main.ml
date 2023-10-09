open Core
open Rinha_t
module Env = Core.Map.Make (String)

type value =
  | Int of int
  | Function of string list * term * env
  | Bool of bool
  | String of string
  | Tuple of value * value

and env = value Env.t
and state = { let_bindings : env }

let rec show_env (env : env) =
  if Map.is_empty env then "empty env"
  else
    Map.fold env ~init:"" ~f:(fun ~key ~data acc ->
        acc ^ key ^ " = " ^ show_value data ^ "\n")

and show_value (v : value) =
  match v with
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | String s -> s
  | Function (_, _, e) -> "function value, env = " ^ show_env e
  | Tuple (f, s) -> "(" ^ show_value f ^ ", " ^ show_value s ^ ")"

let bind_val state name value =
  { let_bindings = Map.set state.let_bindings ~key:name ~data:value }

let concat_closure state closure : state =
  let new_let_bindings =
    Core.Map.fold closure ~init:state.let_bindings ~f:(fun ~key ~data acc ->
        Map.set acc ~key ~data)
  in
  { let_bindings = new_let_bindings }

let show_env (env : env) =
  if Map.is_empty env then "empty env"
  else
    Map.fold env ~init:"" ~f:(fun ~key ~data acc ->
        acc ^ key ^ " = " ^ show_value data ^ "\n")

let print_env (env : env) = print_endline (show_env env)

let rec eval_expr (expr : term) (s : state) : value * state =
  match expr with
  | `Var { text; _ } -> (
      let found_var = Core.Map.find s.let_bindings text in
      match found_var with
      | Some v -> (v, s)
      | None -> failwith ("var not found " ^ text))
  | `Int { value; _ } -> (Int value, s)
  | `Call { callee; arguments; _ } -> (
      let callee, callee_state = eval_expr callee s in
      match callee with
      | Function (params, body, closure) -> (
          let args_and_params = List.zip params arguments in
          match args_and_params with
          | List.Or_unequal_lengths.Unequal_lengths ->
              failwith
                "Function expects N args but you passed a different amount"
          | List.Or_unequal_lengths.Ok args_and_params ->
              let args_resolved =
                List.map
                  ~f:(fun (name, t) ->
                    let evaluated, _ = eval_expr t s in
                    (evaluated, name))
                  args_and_params
              in
              let new_state =
                List.fold args_resolved
                  ~f:(fun acc (p, n) -> bind_val acc n p)
                  ~init:s
              in
              let new_state = concat_closure new_state closure in
              let result, _ = eval_expr body new_state in
              (result, callee_state))
      | _ -> failwith "Cannot call non-callable type")
  | `Let { name; value; next; _ } ->
      let bound, s = eval_expr value s in
      let new_state = bind_val s name.text bound in
      let value_eval, another_state = eval_expr next new_state in

      (value_eval, another_state)
  | `Function { parameters; value; _ } ->
      let params = List.map ~f:(fun x -> x.text) parameters in
      let fval = Function (params, value, s.let_bindings) in
      (fval, s)
  | `If { condition; then_; otherwise; _ } -> (
      let condition, s = eval_expr condition s in
      match condition with
      | Bool true -> eval_expr then_ s
      | Bool false -> eval_expr otherwise s
      | _ -> failwith "Expected 'if' condition to evaluate to a boolean.")
  | `Binary { lhs; rhs; op; _ } -> (
      let lhs, s = eval_expr lhs s in
      let rhs, s = eval_expr rhs s in
      match (lhs, rhs) with
      | Int lhs, Int rhs -> (
          match op with
          | `Add -> (Int (lhs + rhs), s)
          | `Sub -> (Int (lhs - rhs), s)
          | `Mul -> (Int (lhs * rhs), s)
          | `Div -> (Int (lhs / rhs), s)
          | `Lt -> (Bool (lhs < rhs), s)
          | `Gt -> (Bool (lhs > rhs), s)
          | `Lte -> (Bool (lhs <= rhs), s)
          | `Gte -> (Bool (lhs >= rhs), s)
          | `Eq -> (Bool (phys_equal lhs rhs), s)
          | `Neq -> (Bool (not (phys_equal lhs rhs)), s)
          | `Rem -> (Int (lhs % rhs), s)
          | `And -> failwith "AND not supported for int and int"
          | `Or -> failwith "AND not supported for int and int")
      | Bool lhs, Bool rhs -> (
          match op with
          | `Eq -> (Bool (phys_equal lhs rhs), s)
          | `Neq -> (Bool (not (phys_equal lhs rhs)), s)
          | `And -> (Bool (lhs && rhs), s)
          | `Or -> (Bool (lhs || rhs), s)
          | op ->
              let op_str = show_binary_op op in
              failwith (op_str ^ " not supported for bool and bool"))
      | Int lhs, String rhs -> (
          match op with
          | `Add ->
              let concatted = string_of_int lhs ^ rhs in
              (String concatted, s)
          | op ->
              let op_str = show_binary_op op in
              failwith (op_str ^ " not supported for int and string"))
      | String lhs, String rhs -> (
          match op with
          | `Add ->
              let concatted = lhs ^ rhs in
              (String concatted, s)
          | op ->
              let op_str = show_binary_op op in
              failwith (op_str ^ " not supported for string and string"))
      | String lhs, Int rhs -> (
          match op with
          | `Add ->
              let concatted = lhs ^ string_of_int rhs in
              (String concatted, s)
          | op ->
              let op_str = show_binary_op op in
              failwith (op_str ^ " not supported for string and int"))
      | _ -> failwith "unsupported binop")
  | `Bool { value } -> (Bool value, s)
  | `Tuple { first; second; _ } ->
      let f, s = eval_expr first s in
      let sec, s = eval_expr second s in
      (Tuple (f, sec), s)
  | `Str { value; _ } -> (String value, s)
  | `Print { value } ->
      let printed_value, s = eval_expr value s in
      let as_str = show_value printed_value in
      let _ = print_endline as_str in
      (printed_value, s)
  | `First { value; _ } -> (
      let tuple, s = eval_expr value s in
      match tuple with
      | Tuple (f, _) -> (f, s)
      | _ -> failwith "Error: first called on non-tuple value")
  | `Second { value; _ } -> (
      let tuple, s = eval_expr value s in
      match tuple with
      | Tuple (_, sec) -> (sec, s)
      | _ -> failwith "Error: second called on non-tuple value")
  | `Error _ -> failwith "Cannot interpret value of Error"

let () =
  let buf = In_channel.read_all "./out.json" in
  let file = Rinha_j.file_of_string buf in
  (*let _ = print_endline (show_file file) in *)
  let initial_state = Env.empty in
  let state = { let_bindings = initial_state } in

  let eval_result, _ = eval_expr file.expression state in
  print_endline (show_value eval_result)
