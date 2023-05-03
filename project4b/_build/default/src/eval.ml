open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = match e with 
  (* Value: Simply return existing value *)
  | Value x -> x

  (* ID. lookup does most heavly lifting *)
  | ID id -> lookup env id

  (* NOT Unary Operator *)
  | Not e2 -> (let v1 = eval_expr env e2 in match v1 with 
    | Bool b -> Bool (not b)
    | _ -> raise (TypeError ("Expected boolean in NOT unary op")))

  (* BINOP Operations *)
  | Binop (op, e1, e2) -> 
    let check1 = eval_expr env e1 in 
    let check2 = eval_expr env e2 in 
    (match check1 with 
      | Int v1 -> (match check2 with 
        | Int v2 -> (match op with 
          | Add -> Int (v1 + v2)
          | Sub -> Int (v1 - v2)
          | Mult -> Int (v1 * v2)
          | Div -> 
            if v2 = 0 then raise (DivByZeroError)
            else Int (v1 / v2)
          | Greater -> Bool (v1 > v2)
          | Less -> Bool (v1 < v2)
          | GreaterEqual -> Bool (v1 >= v2)
          | LessEqual -> Bool (v1 <= v2)
          | Equal -> Bool (v1 = v2)
          | NotEqual -> Bool (v1 != v2)
          | _ -> raise (TypeError ("Expected int operation")))
        | _ -> raise (TypeError ("Expected check2 to be int")))

      | String s1 -> (match check2 with 
        | String s2 -> (match op with 
          | Concat -> String (s1 ^ s2)
          | Equal -> Bool (s1 = s2)
          | NotEqual -> Bool (s1 <> s2)
          | _ -> raise (TypeError ("Expected String operation")))
        | _ -> raise (TypeError ("Expected check2 to be string")))

      | Bool b1 -> (match check2 with 
        | Bool b2 -> (match op with 
          | Or -> Bool (b1 || b2)
          | And -> Bool (b1 && b2)
          | Equal -> Bool (b1 = b2)
          | NotEqual -> Bool (b1 <> b2)
          | _ -> raise (TypeError ("Expected Boolean operation")))
        | _ -> raise (TypeError ("Expected check to to be boolean")))
      | _ -> raise (TypeError ("Invalid check1 type")))

    (* If expressions *)
    | If (guard, t, f) -> 
      let g = eval_expr env guard in (match g with 
        | Bool b1 -> 
          if b1 then eval_expr env t
          else eval_expr env f
        | _ -> raise (Invalid_argument ("Expected boolean in If guard expression")))

    (* Let expressions COME BACK FOR RECURSIVE CALL *)
    | Let (id, is_rec, init, body) -> 
      if is_rec = false then 
        let v = eval_expr env init in 
        let new_env = (extend env id v) in 
        eval_expr new_env body
      else 
        let temp_env = extend_tmp env id in 
        let v = eval_expr temp_env init in 
        let _ = update temp_env id v in
        eval_expr temp_env body

    (* Anonymous fxns *)
    | Fun (id, body) -> Closure (env, id, body)

    (* Anonymous fxn CALL. Somehow does not handle double fxn calls? *)
    | FunctionCall (c, v) -> (match (eval_expr env c) with 
      | Closure (e, id, body) -> 
        let v2 = (eval_expr env v) in 
        let new_env = (extend e id v2) in 
        (eval_expr new_env body)
      | _ -> raise (TypeError ("Expected fxn but did not find")))

(* Part 2: Evaluating Mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = match m with 
  (* Def operator *)
  | Def (v, e) -> 
    let temp_env = extend_tmp env v in 
    let v2 = eval_expr temp_env e in 
    let _ = update temp_env v v2 in (temp_env, Some v2)
  
  (* Expr Operator *)
  | Expr (e) -> (env, Some (eval_expr env e))

  (* NoOp operator *)
  | NoOp -> (env, None)