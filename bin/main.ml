open Core
open Rinha_t
module Env = Core.Map.Make (String)

type value =
  | Int of int
  | Function of string list * term * env
  | Bool of bool
  | String of string
  | Tuple of value * value

and value_hoas =
  | Int of int (*param names, the function, env*)
  | Function of string list * hoas * env_hoas
  | Bool of bool
  | String of string
  | Tuple of value_hoas * value_hoas

and hoas = value_hoas state -> value_hoas * value_hoas state
and env = value Env.t
and env_hoas = value_hoas Env.t
and 'a state = { let_bindings : 'a Env.t }

let rec show_env : 'a. 'a Env.t -> ('a -> string) -> string =
 fun env show ->
  if Map.is_empty env then "empty env"
  else
    Map.fold env ~init:"" ~f:(fun ~key ~data acc ->
        acc ^ key ^ " = " ^ show data ^ "\n")

and show_value (v : value) =
  match v with
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | String s -> s
  | Function (_, _, e) -> "function value, env = " ^ show_env e show_value
  | Tuple (f, s) -> "(" ^ show_value f ^ ", " ^ show_value s ^ ")"

and show_value_hoas (v : value_hoas) =
  match v with
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | String s -> s
  | Function (_, _, e) -> "function value, env = " ^ show_env e show_value_hoas
  | Tuple (f, s) -> "(" ^ show_value_hoas f ^ ", " ^ show_value_hoas s ^ ")"

let bind_val state name value =
  { let_bindings = Map.set state.let_bindings ~key:name ~data:value }

let concat_closure (state : 'a state) (closure : 'a Env.t) : 'a state =
  let new_let_bindings =
    Core.Map.fold closure ~init:state.let_bindings ~f:(fun ~key ~data acc ->
        Map.set acc ~key ~data)
  in
  { let_bindings = new_let_bindings }

let rec eval_expr (expr : term) (s : value state) : value * value state =
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
  | `Bool { value; _ } -> (Bool value, s)
  | `Tuple { first; second; _ } ->
      let f, s = eval_expr first s in
      let sec, s = eval_expr second s in
      (Tuple (f, sec), s)
  | `Str { value; _ } -> (String value, s)
  | `Print { value; _ } ->
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

let binop_sides s lhs rhs : value_hoas * value_hoas * value_hoas state =
  let lhs, s = lhs s in
  let rhs, s = rhs s in
  (lhs, rhs, s)

let compile_op (lhs : hoas) (rhs : hoas) op : hoas =
 fun s ->
  let lhs, rhs, s = binop_sides s lhs rhs in
  (op lhs rhs, s)

let int_arith op (lhs : value_hoas) (rhs : value_hoas) =
  match (lhs, rhs) with
  | Int lhs, Int rhs -> (Int (op lhs rhs) : value_hoas)
  | _ -> failwith "unsupported op"

let int_comparison op (lhs : value_hoas) (rhs : value_hoas) =
  match (lhs, rhs) with
  | Int lhs, Int rhs -> (Bool (op lhs rhs) : value_hoas)
  | _ -> failwith "unsupported op"

let bool_comparison op (lhs : value_hoas) (rhs : value_hoas) =
  match (lhs, rhs) with
  | Bool lhs, Bool rhs -> (Bool (op lhs rhs) : value_hoas)
  | _ -> failwith "unsupported op"

let generic_comp (int_op : int -> int -> bool) (bool_op : bool -> bool -> bool)
    (lhs : value_hoas) (rhs : value_hoas) =
  match (lhs, rhs) with
  | Int lhs, Int rhs -> (Bool (int_op lhs rhs) : value_hoas)
  | Bool lhs, Bool rhs -> (Bool (bool_op lhs rhs) : value_hoas)
  | _ -> failwith "unsupported op"

let not_eq a b = not @@ phys_equal a b

let rec eval_expr_hoas (expr : term) : hoas =
  match expr with
  | `Var { text; _ } -> (
      fun s ->
        let found_var = Core.Map.find s.let_bindings text in
        match found_var with
        | Some v -> (v, s)
        | None -> failwith ("var not found " ^ text))
  | `Int { value; _ } -> fun s -> (Int value, s)
  | `Call { callee; arguments; _ } -> (
      let callee : hoas = eval_expr_hoas callee in
      let arguments : hoas list =
        List.map ~f:(fun ex -> eval_expr_hoas ex) arguments
      in
      fun s ->
        let callee, s = callee s in
        let original_state = s in
        let params_evaluated : value_hoas list =
          List.map
            ~f:(fun arg ->
              let value, _ = arg s in
              value)
            arguments
        in
        match callee with
        | Function (params, body, closure) -> (
            let args_and_params = List.zip params_evaluated params in
            match args_and_params with
            | List.Or_unequal_lengths.Unequal_lengths ->
                failwith
                  "Function expects N args but you passed a different amount"
            | List.Or_unequal_lengths.Ok args_and_params ->
                let new_state : value_hoas state =
                  List.fold args_and_params
                    ~f:(fun acc (p, n) -> bind_val acc n p)
                    ~init:s
                in
                let new_state = concat_closure new_state closure in
                let (result, _) = body new_state in
                (result, original_state))
        | _ -> failwith "Cannot call non-callable type")
  | `Let { name; value; next; _ } ->
      let value = eval_expr_hoas value in
      let next = eval_expr_hoas next in
      let var_name = name.text in
      fun s ->
        let bound_val, s = value s in
        let s = bind_val s var_name bound_val in
        next s
  | `Function { parameters; value; _ } ->
      let params = List.map ~f:(fun x -> x.text) parameters in
      let body = eval_expr_hoas value in
      fun s ->
        let f : value_hoas = Function (params, body, s.let_bindings) in
        (f, s)
  | `If { condition; then_; otherwise; _ } -> (
      let condition = eval_expr_hoas condition in
      let then_ = eval_expr_hoas then_ in
      let otherwise = eval_expr_hoas otherwise in
      fun s ->
        match condition s with
        | Bool true, s -> then_ s
        | Bool false, s -> otherwise s
        | _ -> failwith "Expected 'if' condition to evaluate to a boolean.")
  | `Binary { lhs; rhs; op; _ } -> (
      let lhs = eval_expr_hoas lhs in
      let rhs = eval_expr_hoas rhs in

      match op with
      | `Add -> (
          fun s ->
            let lhs, rhs, s = binop_sides s lhs rhs in
            match (lhs, rhs) with
            | Int lhs, Int rhs -> (Int (lhs + rhs), s)
            | Int lhs, String rhs -> (String (string_of_int lhs ^ rhs), s)
            | String lhs, Int rhs -> (String (lhs ^ string_of_int rhs), s)
            | String lhs, String rhs -> (String (lhs ^ rhs), s)
            | _ -> failwith "unsupported op")
      | `Sub -> compile_op lhs rhs (int_arith ( - ))
      | `Mul -> compile_op lhs rhs (int_arith ( * ))
      | `Div -> compile_op lhs rhs (int_arith ( / ))
      | `Rem -> compile_op lhs rhs (int_arith ( % ))
      | `Lte -> compile_op lhs rhs (int_comparison ( <= ))
      | `Lt -> compile_op lhs rhs (int_comparison ( < ))
      | `Gte -> compile_op lhs rhs (int_comparison ( >= ))
      | `Gt -> compile_op lhs rhs (int_comparison ( > ))
      | `Eq -> compile_op lhs rhs (generic_comp phys_equal phys_equal)
      | `Neq -> compile_op lhs rhs (generic_comp not_eq not_eq)
      | `And -> compile_op lhs rhs (bool_comparison ( && ))
      | `Or -> compile_op lhs rhs (bool_comparison ( || )))
  | `Bool { value; _ } -> fun s -> (Bool value, s)
  | `Tuple { first; second; _ } ->
      let first = eval_expr_hoas first in
      let second = eval_expr_hoas second in
      fun s ->
        let first, s = first s in
        let second, s = second s in
        (Tuple (first, second), s)
  | `Str { value; _ } -> fun s -> (String value, s)
  | `Print { value; _ } ->
      let value = eval_expr_hoas value in
      fun s ->
        let printed_value, s = value s in
        let as_str = show_value_hoas printed_value in
        let _ = print_endline as_str in
        (printed_value, s)
  | `First { value; _ } -> (
      let tuple = eval_expr_hoas value in
      fun s ->
        match fst (tuple s) with
        | Tuple (f, _) -> (f, s)
        | _ -> failwith "Error: first called on non-tuple value")
  | `Second { value; _ } -> (
      let tuple = eval_expr_hoas value in
      fun s ->
        match fst (tuple s) with
        | Tuple (_, sec) -> (sec, s)
        | _ -> failwith "Error: first called on non-tuple value")
  | `Error _ -> failwith "Cannot compile value of Error"

let measure_time f =
  let start_time =  Time_ns.now () in
  let result = f () in
  let end_time = Time_ns.now () in
  let elapsed_time = Time_ns.diff end_time start_time in
  (result, elapsed_time)

let () =
  let buf = In_channel.read_all "./out.json" in
  let file = Rinha_j.file_of_string buf in

  let _, duration_tw = measure_time (fun () -> 
    let initial_state = Env.empty in
    let state = { let_bindings = initial_state } in
  
    let eval_result, _ = eval_expr file.expression state in
    eval_result
  ) in

  print_endline ("Treewalker took " ^ (Time_ns.Span.to_string duration_tw));
  
  let compiled = eval_expr_hoas file.expression in
  
  let _, duration_hoas = measure_time (fun () -> 
    let initial_state = Env.empty in
    let state = { let_bindings = initial_state } in
  
    let eval_result, _ = compiled state in
    eval_result
  ) in

  print_endline ("HOAS took " ^ (Time_ns.Span.to_string duration_hoas));


  


  ()
