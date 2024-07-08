let rec sum_list l = match l with
| [] -> 0 | h::t -> h+(sum_list t);; 




let rec compress l = match l with
|[] -> l
|f::s::t -> if f=s then compress (f::t) else f::compress (s::t)
|l -> l;;


