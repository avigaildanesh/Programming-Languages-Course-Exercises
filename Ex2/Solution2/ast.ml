type var = string;;
type n = int -> int;;
type state = var -> int ;; 

type a = 
  | Num of n 
  | Var of var 
  | Add of a * a 
  | Mult of a * a 
  | Sub of a * a ;;

type b = 
  | TT 
  | FF 
  | Aeq of a * a
  | Beq of b * b
  | Leq of a * a
  | Neg of b
  | And of b * b ;;

type stm = 
  | Ass of var* a 
  | Skip 
  | Comp of stm * stm 
  | If of b * stm * stm 
  | While of b * stm 
  | If_Ass of var * a * stm * stm
  | Repeat of stm * b;;

