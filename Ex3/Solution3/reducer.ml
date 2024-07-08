(*
  Reducers (interpreters) for lambda-calculus.
*)

open Utils
open Parser


exception OutOfVariablesError


let possible_variables = List.map (fun x -> char_to_string (char_of_int x)) ((range 97 123) @ (range 65 91))


let fresh_var used_vars : string = 
  if StringSet.is_empty (StringSet.diff (string_set_from_list(possible_variables)) used_vars) 
  then raise (OutOfVariablesError)
  else StringSet.choose (StringSet.diff (string_set_from_list(possible_variables)) used_vars)


let rec fv = function
| Variable s -> StringSet.singleton s
| Abstraction (s, t) -> StringSet.remove s (fv t)
| Application (t1, t2) -> StringSet.union (fv t1) (fv t2)


let extract_some = function
  | Some x -> x 
  | _-> None 


let rec substitute x s = function 
| Variable id when id = x -> s 
| Variable id when id != x -> Variable id 
| Abstraction (id, t) when id=x -> Abstraction(id,t) 
| Abstraction (id, t) when (id != x && not(StringSet.mem id (fv s))) -> Abstraction(id, substitute x s t)
| Abstraction (id, t) when (id != x && StringSet.mem id (fv s)) -> let nVar = fresh_var (StringSet.union (StringSet.union (fv s) (fv t)) (StringSet.singleton x)) in Abstraction(nVar	, substitute x s (substitute id (Variable nVar ) t))
| Application (t1,t2) -> Application ((substitute x s t1),(substitute x s t2))
| _-> raise (OutOfVariablesError)


let reduce_cbv = function
  | Application (t1, t2) ->
      begin
        match t1 with
        | Abstraction (x, t3) ->
            Some (substitute x t2 t3)
        | _ ->
            None
      end
  | _ ->
      None


let rec reduce_cbn t = match t with
| Application (t1, t2) -> 
    let t3 = reduce_cbv t1 in (
      match t3 with
        | Some t4 -> Some (Application (t4, t2))
        | _ -> reduce_cbv t
    )
| _ -> None

(*
let rec reduce_cbn = function
  | Application (t1, t2) -> 
      (match reduce_cbv t1 with
      | Some t3 -> Some (Application (t3, t2))
      | None -> None
      )
  | _ -> None
*)