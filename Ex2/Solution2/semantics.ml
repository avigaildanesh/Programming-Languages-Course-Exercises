let default_state = fun _ -> 0;;


let rec aritmetic_semantic exp s = match exp with
  | Num n -> n 1
  | Var x -> s x
  | Add (a1, a2) -> (aritmetic_semantic a1 s) + (aritmetic_semantic a2 s)
  | Mult (a1, a2) -> (aritmetic_semantic a1 s) * (aritmetic_semantic a2 s)
  | Sub (a1, a2) -> (aritmetic_semantic a1 s) - (aritmetic_semantic a2 s);;

let rec boolean_semantic exp s = match exp with
  | TT -> true
  | FF -> false
  | Aeq (a1, a2) -> (aritmetic_semantic a1 s) = (aritmetic_semantic a2 s)
  | Beq (b1, b2) -> (boolean_semantic b1 s) = (boolean_semantic b2 s)
  | Leq (a1, a2) -> (aritmetic_semantic a1 s) <= (aritmetic_semantic a2 s)
  | Neg b' -> not (boolean_semantic b' s)
  | And (b1, b2) -> (boolean_semantic b1 s) && (boolean_semantic b2 s);; 

  let create_state prev_state x y = fun t -> if t=x then aritmetic_semantic y prev_state else prev_state t;; 


let rec nos c = let (exp, s) = c in match exp with 
  | Ass(v,a)-> create_state s v a
  | Skip -> s 
  | Comp(s1,s2) -> let s1_s2 = nos(s1,s) in nos(s2,s1_s2)
  | If(b,s1,s2) -> if boolean_semantic b s then nos(s1,s) else nos(s2,s)
  | While(b,s1) -> if boolean_semantic b s then let sWhile=nos(s1,s) in nos (While(b,s1),sWhile) 
                   else s 
  | If_Ass(v,a,s1,s2) -> let checkAstate= aritmetic_semantic a s in let s3 = nos(Ass(v,a),s) in 
                         if checkAstate=0 then nos(s2,s3) else nos(s1,s3) 
  | Repeat(s1,b) -> let s2= nos(s1,s) in let checkBstate=boolean_semantic b s2 in 
                    if checkBstate then s2 else nos(Repeat(s1,b),s2);; 
