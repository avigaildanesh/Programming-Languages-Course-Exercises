type shape = 
| Circle of float
| Square of float 
| Rectangle of float*float;;

let area s = match s with
|Circle radius -> radius*.radius*.3.14159 
|Square side_len -> side_len *. side_len 
|Rectangle (height,length) -> height*.length ;;

let rec total_area l = match l with 
|[]->0.0
|h::t -> area(h) +. total_area(t);;

