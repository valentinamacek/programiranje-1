(* 1. a) *)
let je_idempotent (*(int * int) * (int * int)*) = function
    | ((a,b),(c,d))-> if a*a + b *c = a && a*b + b*d = b && a*c + d*c = c
                       && b*c + d*d=d then true else false

(* 1. b) *)
let produkt lst = 
    let rec produkt_aux acc = function
    | [] -> acc
    | x :: xs -> if x <> 0 then produkt_aux (x*acc) (xs)
                 else produkt_aux (acc) (xs)
in produkt_aux 1 lst


(* 1. c) *)
let stalin_sort lst = 
    let rec stalin_sort_aux acc novi = function
        | [] -> List.rev novi
        | x :: xs -> match acc with 
                     | Some cac ->  if x > cac then stalin_sort_aux (Some x) (x::novi) (xs)
                                     else stalin_sort_aux (acc) (novi) (xs)
                     | None -> stalin_sort_aux (Some x) (x::novi) (xs)
in stalin_sort_aux None [] lst

(* 1. d) *)
let splosni_collatz f g pogoj start stop = 
    let rec collaps_aux acc x = 
        if x = stop then 
                List.rev acc
        else 
            if pogoj x then
                collaps_aux ((f x)::acc) (f x)
            else 
                collaps_aux ((g x)::acc) (g x)
in collaps_aux [start] start
            

