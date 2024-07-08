let rec helpFun l sum =
  match l with
  | [] -> 0
  | last::[] -> if sum = last then 1 else 0
  | h::t ->
    helpFun t (sum + h) + helpFun t (sum - h);; 
    
let arithmetic_hell l = match l with
| []-> 0
| h::[] -> 0
| h::t -> helpFun t h;;

    

