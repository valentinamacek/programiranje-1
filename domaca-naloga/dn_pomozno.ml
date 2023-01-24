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


type available = { loc : int * int; possible : int list }
(*indeks vrstice * indeks stolpca manjkajocega*, *možni, da ga zasedejo*)


type state = { problem : problem; current_grid : int option grid ; empty_rows : int list ; available: available option}

let osnovni_state = {problem = osnovni ; current_grid = osnovni.initial_grid ; empty_rows = cal_empty_rows (osnovni.initial_grid); available = None }

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


type v_vrstici = {indeksi_none_stolpci: int list ; indeks_vrstice: int ; zasedeni: int list}
(*Doloci pozicijo noneov in kateri so ze v vrstici *)
let ze_v_vrstici (state : state) (index : int ) = 
  let rec ze_v_vrstici_aux acc ind_druge indexi_noneov = function
    | [] -> {indeksi_none_stolpci = indexi_noneov; indeks_vrstice = index; zasedeni = acc }
    | (Some x ) :: xs -> ze_v_vrstici_aux (x:: acc) (ind_druge + 1) (indexi_noneov) (xs)
    | None :: xs -> ze_v_vrstici_aux (acc) (ind_druge + 1) (ind_druge :: indexi_noneov) (xs)
in ze_v_vrstici_aux [] 0 [] (Array.to_list (get_row (state.current_grid) index ) )

type v_stolpcu = {indeksi_none_vrstice: int list ; indeks_stolpca: int ; zasedeni: int list}

let ze_v_stolpcu (state : state) (index : int ) = 
  let rec ze_v_stolpcu_aux acc ind_druge indexi_noneov = function
    | [] -> {indeksi_none_vrstice = indexi_noneov; indeks_stolpca = index; zasedeni = acc }
    | (Some x ) :: xs -> ze_v_stolpcu_aux (x:: acc) (ind_druge + 1) (indexi_noneov) (xs)
    | None :: xs -> ze_v_stolpcu_aux (acc) (ind_druge + 1) (ind_druge :: indexi_noneov) (xs)
in ze_v_stolpcu_aux [] 0 [] (Array.to_list (get_column (state.current_grid) index ) )

let k = ze_v_vrstici osnovni_state 1
(* let manjkajoci (vrst : int list) = *)

(*potrebno še naredit*:*)
(*najde manjkajoce>>*)
(*vstavi manjkajoce*)
(*preveri ce resitecv ustreza*)


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

let poskus = selection_sort k.zasedeni


let manjkajoci (list: int list) = 
  let rec manjkajoci_aux acc = function
      | [] -> Some acc
      | [x] -> Some acc
      | x :: y :: xs -> if x + 1 = y then manjkajoci_aux (acc) (y :: xs) else 
        if x = y then None
        else manjkajoci_aux ((x+1) :: acc) ((x+1)::y::xs)
in manjkajoci_aux [] (selection_sort (0 ::(10) :: list))

let o = ze_v_vrstici osnovni_state 4;; 

(*v branch_state naredimo:
   pogledamo prvo vrstico s praznimi prostori in v njej prvo prazno celico->
     za prazno celico pogledamo kaj manjka v vrstici8ze v podatku iz vrstice, pogledamo se njegov stolpec in manjkajoce v stolpcu->
       state1-> z min(manjkajoci vrstica, manjkajoci stolpec) (za prvic : manjkajoci vrstica)
       state2 -> t max (manjkajoci vrstica, manjkajoci stolpec) (manjkajoci stolpec)
      V solver state:
      z vsako od manjkajocih poskusamo nadaljevati-ena bo privedla do resitve ->
        z moznostmi iz state1(iz available 1) z vsako posebej probamo zapolnit prazno celico in dobimo nova stanja, ki poskusijo it naprej*)

let branch_state_adv (state : state) : (state * state) option = 
    let nepolne_vrstice = Array.of_list (state.empty_rows)
    in let prva_nepolna_vrstica = nepolne_vrstice.(0) 
    in let podatki_o_vrstici =  ze_v_vrstici (state) (prva_nepolna_vrstica)
    in let noni = (Array.of_list (podatki_o_vrstici.indeksi_none_stolpci))
    in let prvi_none = noni.(0)
    in let podatki_o_stolpcu = ze_v_stolpcu (state) (prvi_none)
    in match (manjkajoci (podatki_o_vrstici.zasedeni), manjkajoci (podatki_o_stolpcu.zasedeni) )with
       | (None,_) -> None
       |(_, None) -> None
       | (Some lst1, Some lst2)->let available1 = {loc=(podatki_o_vrstici.indeks_vrstice, prvi_none); possible= lst1}
           in let state1 = {problem= state.problem; current_grid= state.current_grid; empty_rows=state.empty_rows; available= Some available1}
           in let available2 = {loc=(podatki_o_vrstici.indeks_vrstice, prvi_none); possible= lst2} 
          in let state2 = {problem= state.problem; current_grid= state.current_grid; empty_rows=state.empty_rows; available= Some available2}
          in 
          Some (state1, state2)
(*pri obicajnem 1 bosta v eni vrstici 2 moznosti v drugih pa po ena*)
let branch_state (state : state) : (state * state option) option = 
  let nepolne_vrstice = Array.of_list (state.empty_rows)
  in let prva_nepolna_vrstica = nepolne_vrstice.(0) 
  in let podatki_o_vrstici =  ze_v_vrstici (state) (prva_nepolna_vrstica)
  in let noni = (Array.of_list (podatki_o_vrstici.indeksi_none_stolpci))
  in let prvi_none = noni.(0)
  in match manjkajoci (podatki_o_vrstici.zasedeni)with
      | None -> None
      | Some lst->let array_moznih = Array.of_list lst in 
                  if Array.length array_moznih <> 1 then 
                     let  available1 = {loc=(podatki_o_vrstici.indeks_vrstice, prvi_none); possible= [array_moznih.(0)]}
                     in let state1 = {problem= state.problem; current_grid= state.current_grid; empty_rows=state.empty_rows; available= Some available1}
                    in let available2 = {loc=(podatki_o_vrstici.indeks_vrstice, prvi_none); possible= [array_moznih.(1)]}
                    in let state2 = {problem= state.problem; current_grid= state.current_grid; empty_rows=state.empty_rows; available= Some available2}
                    in 
                    Some (state1, Some state2)
                  else 
                    let  available1 = {loc=(podatki_o_vrstici.indeks_vrstice, prvi_none); possible= [array_moznih.(0)]}
                     in let state1 = {problem= state.problem; current_grid= state.current_grid; empty_rows=state.empty_rows; available= Some available1}
                    in 
                    Some (state1, None)
let nova_grid i j element grid = 
  Array.init 9 (fun vrstica -> Array.init 9 (fun st -> if st = j && i = vrstica then (Some element) else grid.(vrstica).(st)))    
                    
let izpolni_grid (state : state) =
    match state.available with 
      | None -> state
      | Some av -> let vrstica, stolpec = av.loc in 
                   let mozni = Array.of_list (av.possible) in
                   let mozni_element = mozni.(0) in
                   let new_grid = nova_grid (vrstica) (stolpec) (mozni_element) (state.current_grid)
                   in 
                   {problem=state.problem; current_grid=new_grid ; empty_rows= cal_empty_rows (new_grid); available=None}  
               
(*veljavnost resitve: 1. ce je v vsakem stolpcu, vrstici in boxu po en element od 1,2,,,,9
                      2. ce se ujema s problem.initial_grid*)
type solution = int grid
let is_valid_solution problem solution = 
    let rec preveri vrstice stolpci = 
      match (vrstice, stolpci) with
      | ([], []) -> true
      | ([], _) -> false
      | (_, []) -> false
      | (x :: xs, y:: ys) -> if (manjkajoci (Array.to_list (x ))= Some []) && (manjkajoci (Array.to_list (y)) = Some []) then preveri (xs) (ys) else false
in preveri (rows solution) (columns solution)

type response = Solved of solution | Unsolved of state | Fail of state

let validate_state (state : state) : response =
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
      explore_state state'

and explore_state (state : state) =
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
  | Some (st1, None) -> solve_state st1 
 
(* type available = { loc : int * int; possible : int list } *)
(*indeks vrstice * indeks stolpca manjkajocega*, *možni, da ga zasedejo*)




(* type state = { problem : problem; current_grid : int option grid ; empty_rows : int list ; available: available option} *)

let osnovni_state = {problem = osnovni ; current_grid = osnovni.initial_grid ; empty_rows = cal_empty_rows (osnovni.initial_grid); available = None }

let osnovni_2state = {problem = osnovni ; current_grid = osnovni.initial_grid ; empty_rows = cal_empty_rows (osnovni.initial_grid); available = Some {loc=(1,4); possible=[4]}}

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

(*da bi hranil list izbranih, do sedaj (in sicer indekse vrstic)--ko bi failal bi sel v pogledat v tazadnji indeks-> ce je tam se kaksna nesprobana permutacija , 
   ce so pa ze vse sprobane gre pa se eno nazaj *)

(*manjkajoci nej se shranijo v tabelo*)
let rec insert el = function
    | [] -> [[el]]
    | x:: xs -> [(el::x::xs)] @ List.map (fun lst -> x :: lst) (insert el xs)
(* let rec nova_per 
mutacija manjkajoci do_sedaj = *)
let rec vse_permutacije manjkajoci = 
  print_endline ("Računam ");
    match manjkajoci with 
    | [] -> [manjkajoci]
    | x:: xs -> List.flatten (List.map (insert x) (vse_permutacije xs))

(*Zaenkrat si shranimo vse_permutacije manjkajocih*)
(* let memoiziraj_rec vse_perm =
  let rezultati = Hashtbl.create 512 in
  let rec mem_f x =
    match Hashtbl.find_opt rezultati x with
    | None ->
        let y = vse_perm mem_f x in
        Hashtbl.add rezultati x y;
        y
    | Some y ->
        y
  in
  mem_f

let mem_perm = memoiziraj_rec (vse_permutacije) *)
(* let ze_v_vrstici (state : state) (index : int ) = 
  let rec ze_v_vrstici_aux acc ind_druge indexi_noneov = function
      | [] -> {indeksi_none_stolpci = indexi_noneov; indeks_vrstice = index; zasedeni = acc }
      | (Some x ) :: xs -> ze_v_vrstici_aux (x:: acc) (ind_druge + 1) (indexi_noneov) (xs)
      | None :: xs -> ze_v_vrstici_aux (acc) (ind_druge + 1) (ind_druge :: indexi_noneov) (xs)
in ze_v_vrstici_aux [] 0 [] (Array.to_list (Model.get_row (state.current_grid) index ) ) *)



type vrste = Vrstica | Stolpec | Box

type lastnosti_objekta = { indeksi_praznih : (int * int) list list ; vrsta : vrste ; zasedeni : int list list }
(*zelimo zabelezit indekse noneov za vsako vrstico posebaj in zabelezit [[indeksi_none 0-te vrstice]; [indeksi_none---]]*)
let izracun_koordinat_iz_boxa box_ind el_v_boxu = 
  let vrstica = box_ind -(box_ind mod 3) + el_v_boxu / 3 in
  let stolpec = (box_ind mod 3) * 3 + el_v_boxu mod 3 in
  (vrstica, stolpec)
  
let v_objektu index arr_objekta vrsta(*je array*) = 
  let rec v_objektu_aux acc_none acc_zasedeni index_druge = function
      | [] -> (acc_none, acc_zasedeni)
      | (Some x ):: xs -> v_objektu_aux (acc_none) (x:: acc_zasedeni) (index_druge + 1) (xs)
      | None :: xs -> match vrsta with 
                      | Vrstica -> v_objektu_aux ((index, index_druge):: acc_none) (acc_zasedeni) (index_druge + 1) (xs)
                      | Stolpec -> v_objektu_aux ((index_druge, index):: acc_none) (acc_zasedeni) (index_druge + 1) (xs)
                      | Box -> let x, y = izracun_koordinat_iz_boxa index index_druge 
                              in v_objektu_aux ((x,y):: acc_none) (acc_zasedeni) (index_druge + 1) (xs)
  in v_objektu_aux [] [] 0 (Array.to_list arr_objekta)

let cal_empty vrsta_objekta grid = 
  let rec empty_rows_aux acc_prazni acc_zasedeni index (*dobis list of arrays*) = function
      | [] -> {indeksi_praznih = List.rev acc_prazni; vrsta=vrsta_objekta; zasedeni= List.rev acc_zasedeni}
      | x :: xs -> if (Array.exists (Option.is_none) x ) then 
                     let prazni, zasedeni = v_objektu (index) (x) (vrsta_objekta) in
                     (empty_rows_aux (prazni:: acc_prazni) (zasedeni :: acc_zasedeni) (index + 1) xs )
                    else 
                      (empty_rows_aux ([]::acc_prazni) ([]::acc_zasedeni) (index + 1) xs)
in 
  match vrsta_objekta with 
      | Vrstica -> empty_rows_aux [] [] 0 (rows grid) 
      | Stolpec -> empty_rows_aux [] [] 0 (columns grid)
      | Box -> empty_rows_aux [] [] 0 (boxes grid)


let kje_nadaljevati vrsta_objekta grid = 
  let rec find_min_aux min min_indeks trenutni_index = function
          | [] -> (min, min_indeks)
          | x :: xs -> if x < min && x <> 0 then find_min_aux (x) (trenutni_index) (trenutni_index + 1) xs
                       else 
                        find_min_aux (min) (min_indeks) (trenutni_index + 1 ) xs
    in 
  let objekt = cal_empty vrsta_objekta grid in
  let elementi = List.map (fun x -> List.length x) (objekt.indeksi_praznih)
  in find_min_aux 9 0 0 elementi

let kje_nadaljevati_v_gridu grid = 
  let rec najmanjsi_od_objektov min vrsta_objekta (min_vrsta_objekta, index)= 
      match vrsta_objekta with
      |Vrstica -> let od_vrstice_min, min_index = kje_nadaljevati Vrstica grid in 
                   najmanjsi_od_objektov (od_vrstice_min) Stolpec (Vrstica, min_index)
      |Stolpec -> let od_stolpca_min, min_index = kje_nadaljevati Stolpec grid in
                  if od_stolpca_min < min then 
                   najmanjsi_od_objektov (od_stolpca_min) Box (Stolpec, min_index)
                  else
                   najmanjsi_od_objektov (min) Box (min_vrsta_objekta, index)
      |Box -> let od_boxa_min, min_index = kje_nadaljevati Box grid in
                  if od_boxa_min < min then 
                    (od_boxa_min, Box, min_index)
                  else
                    (min, min_vrsta_objekta, index)
                    (*vrne: npr: (2, Vrstica, 1)
                       objekt z najmanjsim stevilom praznih celic je vrstica in sicer 1. vrstica ima 2 prazni celici*)
in najmanjsi_od_objektov 9 Vrstica (Vrstica, 0)
(*Vrne nas naslednji korak *)
(* let ze_v_vrstici (state : state) (index : int ) = 
  let rec ze_v_vrstici_aux acc ind_druge indexi_noneov = function
      | [] -> {indeksi_none_stolpci = indexi_noneov; indeks_vrstice = index; zasedeni = acc }
      | (Some x ) :: xs -> ze_v_vrstici_aux (x:: acc) (ind_druge + 1) (indexi_noneov) (xs)
      | None :: xs -> ze_v_vrstici_aux (acc) (ind_druge + 1) (ind_druge :: indexi_noneov) (xs)
in ze_v_vrstici_aux [] 0 [] (Array.to_list (Model.get_row (state.current_grid) index ) ) *)


let poglej_min grid = 
   let min, objekt, indeks = kje_nadaljevati_v_gridu grid in
   let lastnosti = cal_empty objekt grid in
   let stanje_praznih = lastnosti.indeksi_praznih in
   let v_array =  Array.of_list stanje_praznih in
   let prazni = v_array.(indeks) in
   let stanje_zasedeni = lastnosti.zasedeni in 
   let v_arr = Array.of_list stanje_zasedeni in
   let zasedeni = v_arr.(indeks) in
   let manjkajo = manjkajoci zasedeni in
   (prazni, manjkajo)
   (*manjkajo je oblike sez option Al je NONE-> napaka ali pa Some [sez]*)

(*na izpolni morajo prit manjkajoci ze v seznamu*) (*ko manjkajoce na novo izracunamo so prazni*)
(*manjkajoci pridejo v padajoce urejenem seznamu Some [9; 8; 6; 5; 3; 1]*)
(*ideja je postopna permutacija*)
(* let izpolni manjkajoci ze_preizkusene = 
    if  *)
(*imamo indekse praznih  na voljo in manjkajoce -> lahko bi v tem *)
(*Npr. manjkajoci [6;7;8,9] poizkusi najprej *)
(*boljse kot vse permutacije da pogleda, zdajsnji vrstni red *)
(**)
let rec last = function
    | [] -> None
    | x :: [] -> Some x 
    | x :: xs -> last xs
     

let rec backtracking seznam_seznamov = 
  match List.rev seznam_seznamov with 
  | [] -> last []
  | [] :: xs -> backtracking (List.rev xs)(*ce ne ga pol se enkrat obrne*)
  | x :: xs -> last x

  (* # backtracking [[2;3];[4;5]];;ž
  - : int option = Some 5
  # backtracking [[2;3];[4;5];[]];;
  - : int option = Some 5 *)