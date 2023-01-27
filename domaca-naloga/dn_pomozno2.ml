type 'a grid = 'a Array.t Array.t
  ;;
(*int option grid pomeni grid iz int_optionov primer spodaj*)
type 'a grid = 'a Array.t Array.t


let map_grid (f : 'a -> 'b) (grid : 'a grid) : 'b grid = 
  Array.init 9 (fun vrstica -> Array.map f grid.(vrstica))
  

let copy_grid (grid : 'a grid) : 'a grid = map_grid (fun x -> x) grid

let primer = [|[|3;4;5|];[|2;3;6|]|]
  ;;
(* val primer : int array array = [|[|3; 4; 5|]; [|2; 3; 6|]|] *)
 primer.(0)
  ;;
(* - : int array = [|3; 4; 5|] *)
let mapi_grid f grid = 
  Array.init 3 (fun vrstica -> Array.map f grid.(vrstica))
  (* # Array.init 3 (fun i-> primer.(0).(i));;
  - : int array = [|3; 4; 5|] *)
let tabela = [|[|0;0;0;1;1;1;2;2;2|];[|0;0;0;1;1;1;2;2;2|]; [|0;0;0;1;1;1;2;2;2|]; [|3;3;3;4;4;4;5;5;5|];[|3;3;3;4;4;4;5;5;5|]; [|3;3;3;4;4;4;5;5;5|]; [|6;6;6;7;7;7;8;8;8|];[|6;6;6;7;7;7;8;8;8|]; [|6;6;6;7;7;7;8;8;8|]|]
let chunkify size lst =
  let rec aux chunk chunks n lst =
    match (n, lst) with
    | _, [] when chunk = [] -> List.rev chunks
    | _, [] -> List.rev (List.rev chunk :: chunks)
    | 0, _ :: _ -> aux [] (List.rev chunk :: chunks) size lst
    | _, x :: xs -> aux (x :: chunk) chunks (n - 1) xs
  in
  aux [] [] size lst

let get_box grid index =
  let zacetni = index - (index mod 3)
  in
  let row_blocks grid =
  Array.init 3 (fun vrstica -> grid.(zacetni + vrstica)|> Array.to_list |> chunkify 3 |> Array.of_list)
  in 
  let k = row_blocks grid 
  in 
  let l = mapi_grid (Array.of_list) k 
  in
  let razdeljena = Array.init 3 (fun vrstica -> l.(vrstica).(index mod 3)) 
  in Array.concat ( Array.to_list razdeljena)

  let boxes grid = List.init 9 (get_box grid)


  type problem = { initial_grid : int option grid } 

  let i = {initial_grid=[|[|None ; Some 3; Some 4; None|]; [|Some 2; Some 4; Some 5;Some 6|]|]}
  
let unsolved x = Array.exists (Array.exists Option.is_none) x.initial_grid

let j = {initial_grid=[|[|Some 5 ; Some 3; Some 4; Some 7|]; [|Some 2; Some 4; Some 5;Some 6|]|]}

(* # unsolved j;;
- : bool = false
# unsolved i;;
- : bool = true *)
let osnovni = {initial_grid=[|[|Some 4; Some 8; Some 3; Some 9; Some 2; Some 1; Some 6; Some 5; Some 7|];
                              [|Some 9; Some 6; Some 7; Some 3; None ; Some 5; Some 8;Some 2; Some 1 |];
                              [|Some 2; Some 5; Some 1; Some 8; Some 7; Some 6; Some 4; Some 9; Some 3|];
                              [|Some 5; Some 4; Some 8; Some 1; Some 3; Some 2; Some 9; Some 7; Some 6|];
                              [|Some 7; Some 2; Some 9; None ; Some 6; Some 4; None ; Some 3; Some 8|];
                              [|Some 1; Some 3; Some 6; Some 7; Some 9; Some 8; None ; Some 4; Some 5|];
                              [|Some 3; Some 7; Some 2; Some 6; Some 8; Some 9; Some 5; Some 1; Some 4|];
                              [|Some 8; Some 1; Some 4; Some 2; Some 5; Some 3; Some 7; Some 6; Some 9|];
                              [|Some 6; Some 9; Some 5; Some 4; Some 1; Some 7; Some 3; Some 8; Some 2|]
                              |]}

let get_row (grid : 'a grid) (row_ind : int) = 
  Array.init 9 (fun stolpec -> grid.(row_ind).(stolpec))

let rows grid = List.init 9 (get_row grid)

let get_column (grid : 'a grid) (col_ind : int) =
  Array.init 9 (fun row_ind -> grid.(row_ind).(col_ind))

let columns grid = List.init 9 (get_column grid)

let cal_empty_rows grid = 
  let rec empty_rows_aux acc index (*dobis list of arrays*) = function
      | [] -> List.rev acc
      | x :: xs -> if (Array.exists (Option.is_none) x ) then (empty_rows_aux (index :: acc) (index + 1) xs )
                   else (empty_rows_aux (acc) (index + 1) xs)
in empty_rows_aux [] 0 ( rows grid) 


(* type available = { loc : int * int; possible : int list } *)
(*indeks vrstice * indeks stolpca manjkajocega*, *možni, da ga zasedejo*)


(* type state = { problem : problem; current_grid : int option grid ; empty_rows : int list ; available: available option} *)

(* let osnovni_state = {problem = osnovni ; current_grid = osnovni.initial_grid ; empty_rows = cal_empty_rows (osnovni.initial_grid); available = None } *)

(* V state dodas katere vrstice imajo prazne elemente*)

let ena_vrstica = [|Some 4; Some 9; Some 2; Some 5; Some 7; Some 1; Some 3; Some 8; Some 6|]

let malo_prazna_vrstica = [|Some 2; None; Some 7; Some 3; Some 6; Some 9; Some 8; Some 5; Some 1|]
(* 
# rows osnovni_state.current_grid ;;
- : int option array list =
[[|Some 4; Some 8; Some 3; Some 9; Some 2; Some 1; Some 6; Some 5; Some 7|];
 [|Some 9; Some 6; Some 7; Some 3; None; Some 5; Some 8; Some 2; Some 1|];
 [|Some 2; Some 5; Some 1; Some 8; Some 7; Some 6; Some 4; Some 9; Some 3|];
 [|Some 5; Some 4; Some 8; Some 1; Some 3; Some 2; Some 9; Some 7; Some 6|];
 [|Some 7; Some 2; Some 9; None; Some 6; Some 4; None; Some 3; Some 8|];
 [|Some 1; Some 3; Some 6; Some 7; Some 9; Some 8; None; Some 4; Some 5|];
 [|Some 3; Some 7; Some 2; Some 6; Some 8; Some 9; Some 5; Some 1; Some 4|];
 [|Some 8; Some 1; Some 4; Some 2; Some 5; Some 3; Some 7; Some 6; Some 9|];
 [|Some 6; Some 9; Some 5; Some 4; Some 1; Some 7; Some 3; Some 8; Some 2|]] *)

(*Prazne: 1, 4, 5*)

let foldi_grid (f : int -> int -> 'a -> 'acc -> 'acc) (grid : 'a grid)
    (acc : 'acc) : 'acc =
  let acc, _ =
    Array.fold_left
      (fun (acc, row_ind) row ->
        let acc, _ =
          Array.fold_left
            (fun (acc, col_ind) cell ->
              (f row_ind col_ind cell acc, col_ind + 1))
            (acc, 0) row
        in
        (acc, row_ind + 1))
      (acc, 0) grid
  in
  acc

  let min_and_rest list =
    let rec find_min min = function
        | [] -> min
        | x :: xs -> if x < min then find_min x xs else find_min min xs
    in 
    let rec remove_min min prejsnji  = function
        |[] -> failwith "to ni to"
        | x :: xs -> if x = min then (List.rev prejsnji )@ xs else remove_min min (x :: prejsnji)  xs 
    in 
    match list with 
     | [] -> None
     | x :: xs -> 
       let z = find_min x xs in  
       Some( z, remove_min z [] list )
  let selection_sort lst = 
    let rec aux ur neur = 
       match  min_and_rest neur with  
         | None -> List.rev ur 
         | Some(x, xs) -> aux (x :: ur) xs
    in aux [] lst 

  
  
  (* let manjkajoci (list: int list) = 
    let rec manjkajoci_aux acc = function
        | [] -> Some acc
        | [x] -> Some acc
        | x :: y :: xs -> if x + 1 = y then manjkajoci_aux (acc) (y :: xs) else 
          if x = y then None
          else manjkajoci_aux ((x+1) :: acc) ((x+1)::y::xs)
  in manjkajoci_aux [] (selection_sort (0 ::(10) :: list)) *)
  
  

  let nova_grid i j element grid = 
    Array.init 9 (fun vrstica -> Array.init 9 (fun st -> if st = j && i = vrstica then (Some element) else grid.(vrstica).(st)))    
                      
  (* let izpolni_grid (state) =
      match state.available with 
        | None -> state
        | Some av -> let vrstica, stolpec = av.loc in 
                     let mozni = Array.of_list (av.possible) in
                     let mozni_element = mozni.(0) in
                     let new_grid = nova_grid (vrstica) (stolpec) (mozni_element) (state.current_grid)
                     in 
                     {problem=state.problem; current_grid=new_grid ; empty_rows= cal_empty_rows (new_grid); available=None}  
type solution = int grid *)
(* let is_valid_solution problem solution = 
    let rec preveri vrstice stolpci = 
      match (vrstice, stolpci) with
      | ([], []) -> true
      | ([], _) -> false
      | (_, []) -> false
      | (x :: xs, y:: ys) -> if (manjkajoci (Array.to_list (x ))= Some []) && (manjkajoci (Array.to_list (y)) = Some []) then preveri (xs) (ys) else false
in preveri (rows solution) (columns solution) *)

(* type response = Solved of solution | Unsolved of state | Fail of state *)

(* let validate_state (state : state) : response =
  let unsolved =
    Array.exists (Array.exists Option.is_none) state.current_grid
  in
  if unsolved then Unsolved state
  else
    (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
    let solution = map_grid Option.get state.current_grid in
    if is_valid_solution state.problem solution then Solved solution
    else Fail state

let rec solve_state (state : state) =
  (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)
  let novi_state = izpolni_grid state in 
  match validate_state novi_state with
  | Solved solution ->
      (* če smo našli rešitev, končamo *)
      Some solution
  | Fail fail ->
      (* prav tako končamo, če smo odkrili, da rešitev ni *)
      None
  | Unsolved state' ->
      (* če še nismo končali, raziščemo stanje, v katerem smo končali *)
      explore_state state' *)

(* and explore_state (state : state) =
  (* pri raziskovanju najprej pogledamo, ali lahko trenutno stanje razvejimo *)
  match branch_state state with
  | None ->
      (* če stanja ne moremo razvejiti, ga ne moremo raziskati *)
      None
  | Some (st1, Some st2) -> (
      (* če stanje lahko razvejimo na dve možnosti, poizkusimo prvo *)
      match solve_state st1 with
      | Some solution ->
          (* če prva možnost vodi do rešitve, do nje vodi tudi prvotno stanje *)
          Some solution
      | None ->
          (* če prva možnost ne vodi do rešitve, raziščemo še drugo možnost *)
          solve_state st2 )
  | Some (st1, None) -> solve_state st1  *)

(* type available = { loc : int * int; possible : int list } *)
(*indeks vrstice * indeks stolpca manjkajocega*, *možni, da ga zasedejo*)




(* type state = { problem : problem; current_grid : int option grid ; empty_rows : int list ; available: available option} *)

(* let osnovni_state = {problem = osnovni ; current_grid = osnovni.initial_grid ; empty_rows = cal_empty_rows (osnovni.initial_grid); available = None }

let osnovni_2state = {problem = osnovni ; current_grid = osnovni.initial_grid ; empty_rows = cal_empty_rows (osnovni.initial_grid); available = Some {loc=(1,4); possible=[4]}} *)

let string_of_list string_of_element sep lst =
  lst |> List.map string_of_element |> String.concat sep

let string_of_nested_list string_of_element inner_sep outer_sep =
  string_of_list (string_of_list string_of_element inner_sep) outer_sep

let string_of_row string_of_cell row =
  let string_of_cells =
    row |> Array.to_list |> chunkify 3
    |> string_of_nested_list string_of_cell "" "│"
  in
  "┃" ^ string_of_cells ^ "┃\n"

let print_grid string_of_cell grid =
  let ln = "───" in
  let big = "━━━" in
  let divider = "┠" ^ ln ^ "┼" ^ ln ^ "┼" ^ ln ^ "┨\n" in
  let row_blocks =
    grid |> Array.to_list |> chunkify 3
    |> string_of_nested_list (string_of_row string_of_cell) "" divider
  in
  Printf.printf "┏%s┯%s┯%s┓\n" big big big;
  Printf.printf "%s" row_blocks;
  Printf.printf "┗%s┷%s┷%s┛\n" big big big

let row_of_string cell_of_char str =
  List.init (String.length str) (String.get str) |> List.filter_map cell_of_char

let grid_of_string cell_of_char str =
  let grid =
    str |> String.split_on_char '\n'
    |> List.map (row_of_string cell_of_char)
    |> List.filter (function [] -> false | _ -> true)
    |> List.map Array.of_list |> Array.of_list
  in
  if Array.length grid <> 9 then failwith "Nepravilno število vrstic";
  if Array.exists (fun x -> x <> 9) (Array.map Array.length grid) then
    failwith "Nepravilno število stolpcev";
  grid

let print_problem problem : unit = 
  print_grid (
    fun c -> match c with 
      | None -> " " 
      | Some i -> string_of_int i
  ) problem.initial_grid


let problem_of_string str =
  let cell_of_char = function
    | ' ' -> Some None
    | c when '1' <= c && c <= '9' -> Some (Some (Char.code c - Char.code '0')) (*vrne int 9 npr.*)
    | _ -> None
  in
  { initial_grid = grid_of_string cell_of_char str }

let print_solution solutione  = 
    match solutione with 
    | None -> ()
    | Some solution -> print_grid (fun i -> string_of_int i) solution
                     
                     
let  row_blocks grid =
    Array.init 3 (fun vrstica -> grid.(vrstica)|> Array.to_list |> chunkify 3 |> Array.of_list)

    let read_problem filename =
    let channel = open_in filename in
    let str = really_input_string channel (in_channel_length channel) in
    close_in channel;
    problem_of_string str

let osnovni2 = read_problem "D:\\programiranje_1\\programiranje-1\\domaca-naloga\\sudokuji\\obicajni-2.sdk" 
let osnovni3 = read_problem "D:\\programiranje_1\\programiranje-1\\domaca-naloga\\sudokuji\\obicajni-9.sdk"

let manjkajoci (list: int list) = 
  let rec manjkajoci_aux acc = function
      | [] -> acc
      | [x] -> acc
      | x :: y :: xs -> if x + 1 = y then manjkajoci_aux (acc) (y :: xs)
        else manjkajoci_aux ((x+1) :: acc) ((x+1)::y::xs)
in manjkajoci_aux [] (selection_sort (0 ::(10) :: list))


type objekt = Vrstica | Stolpec | Box 

type lastnosti_objekta = { indeksi_praznih : (int * int) list array ; vrsta : objekt ; manjkajoci : int list array ; min_dolzina : int ; min_indeksi: int list}
(*zelimo zabelezit indekse noneov za vsako vrstico posebaj in zabelezit [[indeksi_none 0-te vrstice]; [indeksi_none---]]*)
let izracun_koordinat_iz_boxa box_ind el_v_boxu = 
  let vrstica = box_ind -(box_ind mod 3) + el_v_boxu / 3 in
  let stolpec = (box_ind mod 3) * 3 + el_v_boxu mod 3 in
  (vrstica, stolpec)
  
let v_objektu index arr_objekta vrsta(*je array*) = 
  let rec v_objektu_aux acc_none acc_zasedeni index_druge = function
      | [] ->   (acc_none, manjkajoci acc_zasedeni )
      | (Some x ):: xs -> v_objektu_aux (acc_none) (x:: acc_zasedeni) (index_druge + 1) (xs)
      | None :: xs -> match vrsta with 
                      | Vrstica -> v_objektu_aux ((index, index_druge):: acc_none) (acc_zasedeni) (index_druge + 1) (xs)
                      | Stolpec -> v_objektu_aux ((index_druge, index):: acc_none) (acc_zasedeni) (index_druge + 1) (xs)
                      | Box -> let x, y = izracun_koordinat_iz_boxa index index_druge 
                              in v_objektu_aux ((x,y):: acc_none) (acc_zasedeni) (index_druge + 1) (xs)
  in v_objektu_aux [] [] 0 (Array.to_list arr_objekta)

let cal_empty vrsta_objekta grid = 
  let rec empty_rows_aux acc_prazni acc_manjkajoci min min_indexi index (*dobis list of arrays*) = function
      | [] -> {indeksi_praznih = Array.of_list (List.rev acc_prazni); vrsta=vrsta_objekta; manjkajoci= Array.of_list (List.rev acc_manjkajoci);
                min_dolzina= min; min_indeksi=min_indexi}
      | x :: xs -> if (Array.exists (Option.is_none) x ) then 
                     let prazni, manjkajoci = v_objektu (index) (x) (vrsta_objekta) in
                        let dolzina_manjkajocih = List.length (manjkajoci) in
                            if dolzina_manjkajocih < min then
                              (empty_rows_aux (prazni:: acc_prazni) (manjkajoci :: acc_manjkajoci) (dolzina_manjkajocih) ([index]) (index + 1) xs )
                            else
                              if dolzina_manjkajocih = min then 
                                (empty_rows_aux (prazni:: acc_prazni) (manjkajoci :: acc_manjkajoci) (min) (index :: min_indexi) (index + 1) xs )
                              else
                                (empty_rows_aux (prazni:: acc_prazni) (manjkajoci :: acc_manjkajoci) (min) (min_indexi) (index + 1) xs )
                    else 
                      (empty_rows_aux ([]::acc_prazni) ([]::acc_manjkajoci) (min) (min_indexi) (index + 1) xs)
in 
  match vrsta_objekta with 
      | Vrstica -> empty_rows_aux [] []  9 [] 0 (rows grid) 
      | Stolpec -> empty_rows_aux [] [] 9 [] 0 (columns grid)
      | Box -> empty_rows_aux [] [] 9 [] 0 (boxes grid)






(*assert da to racunamo samo na zacetku (ko so fix vsi elemnti razicni)*)
let presek3eh stolpec vrstica box = 
    let rec presek3eh_aux acc = function
        | (_,[],_) -> acc
        | (_,_,[]) -> acc
        | ([],_,_) -> acc
        | (x::xs, y::ys, z::zs) -> if x=y && y=z then presek3eh_aux (x::acc) (xs,ys,zs)
                                  else let novix = ref xs in
                                   let noviy = ref ys in
                                   let noviz = ref zs in
                                   let min_dveh = min x y in 
                                   let mini = min min_dveh z in
                                   if x = mini then novix := x :: !novix; 
                                   if y = mini then noviy := y :: !noviy;
                                   if z = mini then noviz := z :: !noviz;
                                   presek3eh_aux (acc) (!novix, !noviy, !noviz)
in presek3eh_aux [] (stolpec,vrstica, box)




(* 
If you want to have a sequence of if statements, each if must return unit. You can use a reference to your list to make it mutable, put semicolons after each if statement and return the referenced list at the end:

let f list =
  let list = ref list in
  if cond1 then list := 1 :: !list;
  if cond2 then list := 2 :: !list;
  [...]
  !list
;; *)



(* let prazne_moznosti_iz_boxa sez_noneov manjka_v_vrstici manjka_v_stolpcu manjka_v_boxu = 
    let rec prazne_moznosti_aux acc = function
        | [] -> acc
        | (x,y) :: xs -> let skupni = presek3eh (manjka_v_vrstici.(x)) (manjka_v_stolpcu.(y)) (manjka_v_boxu.()) in 
                         let podatekxy = {loc=(x,y); possible=skupni}
                         in 
                         prazne_moznosti_aux (podatekxy::acc) (xs)
in prazne_moznosti_aux [] sez_noneov *)
(* 
let mozni_za_prazno grid = 
  let vrstice = cal_empty Vrstica grid in 
  let manjka_v_vrstici = vrstice.manjkajoci in
  let stolpci = cal_empty Stolpec grid in 
  let manjka_v_stolpcu = stolpci.manjkajoci in
  let box = cal_empty Box grid in 
  let rec mozni_za_none acc index = function 
      | [] -> List.flatten acc
      | x :: xs -> let prazni_boxij  = prazne_moznosti_iz_boxa (x) (manjka_v_vrstici) (manjka_v_stolpcu) (box.manjkajoci.(index)) 
                  in 
                  mozni_za_none (prazni_boxij :: acc) (index +1) xs
in mozni_za_none [] 0 (Array.to_list (box.indeksi_praznih))                                *)


(* let najkrajsi objekt_state =
  let dolzine = objekt_state.manjkajoci in *)

   

(* let izracunaj_moznosti grid = 
  let minimalen = minimalna_dolzina_manjkajocih in *)

let izracun_boxa_iz_koordinat x y = 
    x - x mod 3 + y/3

let f x = 
  let z = ref x in
  let k= !z + 5 in
   z:= k;
  (!z)



(*Deluje*)
(* let f grid = 
  kje_zaceti grid + 1  *)
(* let kje_nadaljevati vrsta_objekta grid = 
  let objekt = cal_empty vrsta_objekta grid in
  let manjkajo = objekt.manjkajoci in 
  let min = ref 9 in
  let min_indeks = ref [] in
  for i = 0 to 9 do 
    let dolzina_manjkajocih = List.length (manjkajo.(i)) in
      if (dolzina_manjkajocih) < (!min) && dolzina_manjkajocih > 0 then 
        min := dolzina_manjkajocih
        min_indeks := [i];
      if dolzina_manjkajocih = !min then 
        min_indeks := i::min_index;
  done;
  (!min, !min_indeks) *)

type available = { loc : int * int; possible : int list }

type statenew = { problem : problem; current_grid : int option grid ; vrstice : lastnosti_objekta ; stolpci : lastnosti_objekta ; boxi : lastnosti_objekta; mutable minimalni: (int list * objekt) list;  mutable minimalen: int; mutable za_resevanje: available list} 

let state2 = {problem =osnovni2; current_grid=osnovni2.initial_grid; vrstice= cal_empty Vrstica osnovni2.initial_grid ; stolpci= cal_empty Stolpec osnovni2.initial_grid; boxi= cal_empty Box osnovni2.initial_grid ; za_resevanje=[] ; minimalni=[([],Vrstica)]; minimalen=9}

let daj_na_izracun objekt = 
  let rec daj_na_izracun_aux acc = function
      | [] -> List.flatten acc
      | x :: xs-> daj_na_izracun_aux ((objekt.indeksi_praznih.(x))::acc) xs 
in daj_na_izracun_aux [] objekt.min_indeksi

let minimalna_dolzina_manjkajocih (state) = 
  let vrstice = cal_empty Vrstica state.current_grid in
  let minimalen = ref vrstice.min_dolzina in
  let stolpci = cal_empty Stolpec state.current_grid in 
    if stolpci.min_dolzina < !minimalen then 
      minimalen:= stolpci.min_dolzina;
  let boxi = cal_empty Box state.current_grid in 
  if boxi.min_dolzina < !minimalen then 
    minimalen:= boxi.min_dolzina;
  state.minimalen <- !minimalen


(* let prazne_moznosti sez_noneov manjkajoci_v_vrsticah manjkajoci_v_stolpcih manjkajoci_v_boxih = 
    let rec prazne_moznosti_aux acc = function
        | [] -> acc
        | (x,y) :: xs -> let skupni = presek3eh (manjkajoci_v_vrsticah.(x)) (manjkajoci_v_stolpcih.(y)) (manjkajoci_v_boxih.((izracun_boxa_iz_koordinat x y))) in 
                          let podatekxy = {loc=(x,y); possible=skupni}
                          in 
                          prazne_moznosti_aux (podatekxy::acc) (xs)
in prazne_moznosti_aux [] sez_noneov *)



(*Popravi*)
(*na izracun lahko grejo isti indeksi veckrat*)
(*for x in manjkajoci count the number of possible places*)
(* let najboljsa_moznost (state: statenew) = 
  let  *)
(* let najboljsa_moznost (state:statenew) = 
   let min = ref  in 
   if state.vrstice.min_dolzina = state.minimalen then 
     let x,y = minimalni  Vrstica state in
        if x < min then 

   if state.stolpci.min_dolzina = state.minimalen then *)
let zapisi_minimalne (state)=
   let mini = ref [] in
   if state.vrstice.min_dolzina = state.minimalen then 
     mini := (state.vrstice.min_indeksi, Vrstica)::!mini;
   if state.stolpci.min_dolzina = state.minimalen then 
    mini := (state.stolpci.min_indeksi, Stolpec)::!mini;
   if state.boxi.min_dolzina = state.minimalen then
    mini := (state.boxi.min_indeksi, Box)::!mini; 
  state.minimalni <- !mini

(* let daj_na_izracun objekt = 
  let rec daj_na_izracun_aux acc = function
      | [] -> List.flatten acc
      | x :: xs-> daj_na_izracun_aux ((objekt.indeksi_praznih.(x))::acc) xs 
in daj_na_izracun_aux [] objekt.min_indeksi *)



let prazne_moznosti sez_noneov (state) = 
  let rec prazne_moznosti_aux acc = function
      | [] -> acc
      | (x,y) :: xs -> let skupni = presek3eh (state.vrstice.manjkajoci.(x)) (state.stolpci.manjkajoci.(y)) (state.boxi.manjkajoci.((izracun_boxa_iz_koordinat x y))) in 
                        let podatekxy = {loc=(x,y); possible=skupni}
                        in 
                        prazne_moznosti_aux (podatekxy::acc) (xs)
in prazne_moznosti_aux [] sez_noneov


let za_vsak_min_objekt_izracunaj indeksi (objekt) (state)= 
  let rec daj_na_izracun_aux acc = function
     | [] -> acc
     | x::xs ->let rez = prazne_moznosti (objekt.indeksi_praznih.(x)) (state) in
                daj_na_izracun_aux (rez::acc) (xs)
in daj_na_izracun_aux [] indeksi 

let za_vse_minimalne (state) = 
  let rec za_vse_minimalne_aux acc = function
      | [] -> acc
      | (x,y)::xs -> match y with 
                      | Vrstica -> let vsi_od_vrstic = za_vsak_min_objekt_izracunaj (x) (state.vrstice) (state) in
                                    za_vse_minimalne_aux (vsi_od_vrstic::acc) xs
                      | Stolpec -> let vsi_od_stolpcev = za_vsak_min_objekt_izracunaj (x) (state.stolpci) (state) in
                      za_vse_minimalne_aux (vsi_od_stolpcev::acc) xs
                      | Box -> let vsi_od_boxov = za_vsak_min_objekt_izracunaj (x) (state.boxi)  (state) in
                      za_vse_minimalne_aux (vsi_od_boxov::acc) xs
in za_vse_minimalne_aux [] state.minimalni
(* let boljse_moznosti (state) = 
  let rec boljse_moznosti_aux acc = function
      | [] -> acc
      | (x,y) :: xs ->  *)
(* let unija_elementov_seznamov_narascajoce_urejenih  seznam1 seznam2 = 
    let zacetni= seznam 1 in  *)
(*Rabimo : unija seznamov
          odstranjevanje elementov v seznamu
          ce je element vsebovan v seznamu najhitrejse*)

(* 
  let zapisi_minimalne (state) in 
in boljse_moznosti_aux [] state.minimalni *)





(*samo za vsako vrstico box in stolpec ki je min mors to narest*)
(*for manjkajoci in min_objekt count possible places -if 1 resi or if possible 1 *)
