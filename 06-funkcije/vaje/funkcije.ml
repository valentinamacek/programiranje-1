(* ========== Vaja 2: Funkcijsko Programiranje  ========== *)

(*----------------------------------------------------------------------------*]
 Definirajte pomožno funkcijo za obračanje seznamov.
[*----------------------------------------------------------------------------*)

let rec reverse = function
  | [] -> [] 
  | x:: xs-> reverse xs @ [x]

(*----------------------------------------------------------------------------*]
 Funkcija [repeat x n] vrne seznam [n] ponovitev vrednosti [x]. Za neprimerne
 vrednosti [n] funkcija vrne prazen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # repeat "A" 5;;
 - : string list = ["A"; "A"; "A"; "A"; "A"]
 # repeat "A" (-2);;
 - : string list = []
[*----------------------------------------------------------------------------*)

let repeat x n = 
  let rec repeat_aux acc x m = 
    if m > 0 then 
      repeat_aux (x :: acc) x (m-1) 
    else  
      acc
  in
  repeat_aux [] x n
(*----------------------------------------------------------------------------*]
 Funkcija [range] sprejme število in vrne seznam vseh celih števil od 0 do
 vključno danega števila. Za neprimerne argumente funkcija vrne prazen seznam.
 Funkcija je repno rekurzivna.
Pri tem ne smete uporabbiti vgrajene funkcije [List.init].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # range 10;;
 - : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
[*----------------------------------------------------------------------------*)

let range n = 
  let rec range_aux acc m = 
    if m >= 0 then 
      range_aux (m :: acc) (m-1)
    else 
      acc 
  in 
  range_aux [] n
  

(*----------------------------------------------------------------------------*]
 Funkcija [map f list] sprejme seznam [list] oblike [x0; x1; x2; ...] in
 funkcijo [f] ter vrne seznam preslikanih vrednosti, torej
 [f x0; f x1; f x2; ...].
 Pri tem ne smete uporabiti vgrajene funkcije [List.map].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (+) 2 in
   map plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

let map f sez = 
  let rec map_aux acc  = function
    | [] -> acc
    | x :: xs -> map_aux ((f x):: acc) xs
  in 
  let obrnjen = reverse sez 
  in 
  map_aux [] obrnjen 

(*----------------------------------------------------------------------------*]
 Časovna zahtevnost operatorja [@] je linearna v prvem argumentu, poskušajte 
 napisati reverse tako, da bo bolj učinkovit in hkrati repno rekurziven.
 Pri tem ne smete uporabiti vgrajene funkcije [List.rev] ali [List.rev_append].
[*----------------------------------------------------------------------------*)
let reverse2 lst = 
  let rec reverse_aux acc = function
    | [] -> acc
    | x :: xs -> reverse_aux (x:: acc) xs 
  in 
  reverse_aux  [] lst

(*----------------------------------------------------------------------------*]
 Funkcija [map_tlrec] je repno rekurzivna različica funkcije [map].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (fun x -> x + 2) in
   map_tlrec plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

let rec map_tlrec f sez = 
  let rec map_aux acc  = function
    | [] -> acc
    | x :: xs -> map_aux (f x :: acc)  (xs)
  in
  let obrnjen = reverse2 sez 
  in 
  map_aux [] obrnjen 
    (* je repno rekurzivna, ce je tista ki jo poklice repno rekurzivna*)
(*----------------------------------------------------------------------------*]
 Funkcija [mapi] je ekvivalentna python kodi:

  def mapi(f, list):
      mapi_list = []
      index = 0
      for x in list:
          mapi_list += [f(x, index)]
          index += 1
      return mapi_list

 Pri tem ne smete uporabiti vgrajene funkcije [List.mapi].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mapi (+) [0; 0; 0; 2; 2; 2];;
 - : int list = [0; 1; 2; 5; 6; 7]
[*----------------------------------------------------------------------------*)
let mapi f sez = 
  let rec mapi_aux acc index = function 
    | []-> reverse2 acc
    | x :: xs -> mapi_aux ((f x index):: acc) (index + 1 ) (xs)
  in 
  mapi_aux [] 0 sez
      
    

  
  

(*----------------------------------------------------------------------------*]
 Funkcija [zip] sprejme dva seznama in vrne seznam parov istoležnih
 elementov podanih seznamov. Če seznama nista enake dolžine vrne napako.
 Pri tem ne smete uporabiti vgrajene funkcije [List.combine].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # zip [1; 1; 1; 1] [0; 1; 2; 3];;
 - : (int * int) list = [(1, 0); (1, 1); (1, 2); (1, 3)]
 # zip [1; 1; 1; 1] [1; 2; 3; 4; 5];;
 Exception: Failure "Different lengths of input lists.".
[*----------------------------------------------------------------------------*)
let dolzina sez = 
  let rec dolzina_aux acc = function
    | [] -> acc
    | x :: xs-> dolzina_aux (acc + 1) xs
  in 
  dolzina_aux 0 sez 
    
let zip sez1 sez2 = 
  let rec zip_aux acc = function 
    | [[], []]-> reverse2 acc
    | [(x::xs), (i::xi)] -> zip_aux ((x,i)::acc) [xs, xi] 
    | _ -> failwith "Different lengths of input lists."
  in 
  zip_aux [] [sez1, sez2]
    
(*----------------------------------------------------------------------------*]
 Funkcija [unzip] je inverz funkcije [zip], torej sprejme seznam parov
 [(x0, y0); (x1, y1); ...] in vrne par seznamov ([x0; x1; ...], [y0; y1; ...]).
 Pri tem ne smete uporabiti vgrajene funkcije [List.split].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)
(* na w je | komenti enako kot py*)
let rec unzip sez = 
  let rec unzip_aux acc1 acc2 = function
  | [] -> ( reverse2 acc1, reverse2 acc2)
  | (x, y) :: xs -> unzip_aux  (x:: acc1) (y :: acc2) xs
  in 
  unzip_aux [] [] sez

(*----------------------------------------------------------------------------*]
 Funkcija [unzip_tlrec] je repno rekurzivna različica funkcije [unzip].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip_tlrec [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let rec unzip_tlrec = ()

(*----------------------------------------------------------------------------*]
 Funkcija [loop condition f x] naj se izvede kot python koda:

  def loop(condition, f, x):
      while condition(x):
          x = f(x)
      return x

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # loop (fun x -> x < 10) ((+) 4) 4;;
 - : int = 12
[*----------------------------------------------------------------------------*)

let  loop boo f x = 
  let rec loop_aux y = 
    if boo y   then
      loop_aux (f y)
    else
      y
  in
  loop_aux x 
(*----------------------------------------------------------------------------*]
 Funkcija [fold_left_no_acc f list] sprejme seznam [x0; x1; ...; xn] in
 funkcijo dveh argumentov [f] in vrne vrednost izračuna
 f(... (f (f x0 x1) x2) ... xn).
 V primeru seznama z manj kot dvema elementoma vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # fold_left_no_acc (^) ["F"; "I"; "C"; "U"; "S"];;
 - : string = "FICUS"
[*----------------------------------------------------------------------------*)
    (*tip folda*)
    (* (a->a->a) -> a list -> a  *)
let rec fold_left_no_acc f = function 
  | x :: y :: []-> f x y 
  | x :: y :: tail -> fold_left_no_acc f (f x y :: tail)
  | [] | _ -> failwith "Seznam je prekratek" 
                     

(*----------------------------------------------------------------------------*]
 Funkcija [apply_sequence f x n] vrne seznam zaporednih uporab funkcije [f] na
 vrednosti [x] do vključno [n]-te uporabe, torej
 [x; f x; f (f x); ...; (f uporabljena n-krat na x)].
 Funkcija je repno rekurzivna.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# apply_sequence (fun x -> x * x) 2 5;;
- : int list = [2; 4; 16; 256; 65536; 4294967296]
               # apply_sequence (fun x -> x * x) 2 (-5);;
- : int list = []
  [*----------------------------------------------------------------------------*)

let apply_sequence f x n = 
  let rec apply_sequence_aux  acc x m = 
    if m > 0 then 
      apply_sequence_aux (x::acc) (f x) (m-1)
    else
      reverse2 (x :: acc)
  in 
  apply_sequence_aux [] x n

(* ta zadni se se ne shrani, ker gre f x istocasno navzdol kot n , seznam pa z zamikom za ena *)
(*----------------------------------------------------------------------------*]
 Funkcija [filter f list] vrne seznam elementov [list], pri katerih funkcija [f]
 vrne vrednost [true].
 Pri tem ne smete uporabiti vgrajene funkcije [List.filter].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # filter ((<)3) [0; 1; 2; 3; 4; 5];;
 - : int list = [4; 5]
[*----------------------------------------------------------------------------*)

let rec filter boo sez =
  let rec filter_aux acc = function
    | [] -> reverse2 acc
    | x :: xs -> if boo x then filter_aux (x :: acc) xs  else filter_aux acc xs
  in
  filter_aux [] sez 

(*----------------------------------------------------------------------------*]
 Funkcija [exists] sprejme seznam in funkcijo, ter vrne vrednost [true] čim
 obstaja element seznama, za katerega funkcija vrne [true] in [false] sicer.
 Funkcija je repno rekurzivna.
 Pri tem ne smete uporabiti vgrajene funkcije [List.find] ali podobnih.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # exists ((<) 3) [0; 1; 2; 3; 4; 5];;
 - : bool = true
 # exists ((<) 8) [0; 1; 2; 3; 4; 5];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec exists f sez = 
  let rez = filter f sez
  in 
  if rez = [] then false else true


(*----------------------------------------------------------------------------*]
 Funkcija [first f default list] vrne prvi element seznama, za katerega
 funkcija [f] vrne [true]. Če takšnega elementa ni, vrne [default].
 Funkcija je repno rekurzivna.
 Pri tem ne smete uporabiti vgrajene funkcije [List.find] ali podobnih. 
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # first ((<) 3) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 5
 # first ((<) 8) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 0
[*----------------------------------------------------------------------------*)
let rec first f default = function
  | [] -> default 
  | x :: xs -> if f x then x else first (f) (default) (xs)

