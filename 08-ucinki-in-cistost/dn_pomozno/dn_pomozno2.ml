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

  (* let min_and_rest list =
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
    in aux [] lst  *)

  
  
  (* let manjkajoci (list: int list) = 
    let rec manjkajoci_aux acc = function
        | [] -> Some acc
        | [x] -> Some acc
        | x :: y :: xs -> if x + 1 = y then manjkajoci_aux (acc) (y :: xs) else 
          if x = y then None
          else manjkajoci_aux ((x+1) :: acc) ((x+1)::y::xs)
  in manjkajoci_aux [] (selection_sort (0 ::(10) :: list)) *)
  type objekt = Vrstica | Stolpec | Box 

  type lastnosti_objekta = { indeksi_praznih : (int * int) list array ; vrsta : objekt ; manjkajoci : int list array }

  type available = { loc : int * int; mutable possible : int list }

  type statenew = { problem : problem; current_grid : int option grid ; vrstice : lastnosti_objekta ; stolpci : lastnosti_objekta ; boxi : lastnosti_objekta; mutable za_resevanje: available array array array} 
  

 
                      
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

let osnovni6 = read_problem "D:\\programiranje_1\\programiranje-1\\domaca-naloga\\sudokuji\\obicajni-6.sdk"
let osnovni49 = read_problem "D:\\programiranje_1\\programiranje-1\\domaca-naloga\\sudokuji\\obicajni-49.sdk"
let osnovni55 = read_problem "D:\\programiranje_1\\programiranje-1\\domaca-naloga\\sudokuji\\obicajni-55.sdk"
let osnovni60 = read_problem "D:\\programiranje_1\\programiranje-1\\domaca-naloga\\sudokuji\\obicajni-60.sdk"
let osnovni100 = read_problem "D:\\programiranje_1\\programiranje-1\\domaca-naloga\\sudokuji\\obicajni-100.sdk"

let manjkajoci (list: int list) = 
  let rec manjkajoci_aux acc = function
      | [] -> acc
      | [x] -> acc
      | x :: y :: xs -> if x + 1 = y then manjkajoci_aux (acc) (y :: xs)
        else manjkajoci_aux ((x+1) :: acc) ((x+1)::y::xs)
in  
let za_uredit = 0::10::list in
manjkajoci_aux [] (List.sort compare za_uredit)

(* let manjkajoci (list: int list) = 
  let rec manjkajoci_aux acc = function
      | [] -> acc
      | [x] -> x :: acc
      | x :: y :: xs -> if x + 1 = y then manjkajoci_aux acc (y :: xs)
        else manjkajoci_aux ((x :: acc) @ [y]) ((x+1) :: (y-1) :: xs)
  in List.rev (manjkajoci_aux [] (List.sort (0 :: (10) :: list))) *)

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
  let rec cal_empty_aux acc_prazni acc_manjkajoci index (*dobis list of arrays*) = function
      | [] -> {indeksi_praznih = Array.of_list (List.rev acc_prazni); vrsta=vrsta_objekta; manjkajoci= Array.of_list (List.rev acc_manjkajoci);}
      | x :: xs -> if (Array.exists (Option.is_none) x ) then 
                     let prazni, manjkajoci = v_objektu (index) (x) (vrsta_objekta) in
                         (cal_empty_aux (prazni:: acc_prazni) (manjkajoci :: acc_manjkajoci)  (index + 1) xs )
                    else 
                      (cal_empty_aux ([]::acc_prazni) ([]::acc_manjkajoci) (index + 1) xs)
in 
  match vrsta_objekta with 
      | Vrstica -> cal_empty_aux [] []  0 (rows grid) 
      | Stolpec -> cal_empty_aux [] [] 0 (columns grid)
      | Box -> cal_empty_aux [] [] 0 (boxes grid)






(*assert da to racunamo samo na zacetku (ko so fix vsi elemnti razicni)*)
(*Ker dobimo manjkajoce v padajocem vrstnem redu in sto funkcijo pregledujemo glave od vecje k manjsi je acc urejen narascajoce*)
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






let initialize_state (problem : problem) =
  { problem=problem ;  current_grid = copy_grid (problem.initial_grid); vrstice=cal_empty Vrstica problem.initial_grid; stolpci= cal_empty Stolpec problem.initial_grid; boxi=cal_empty Box problem.initial_grid ;za_resevanje=[||] }



let s6 = initialize_state osnovni6
let s49 = initialize_state osnovni49
let s55 = initialize_state osnovni55
let s60 = initialize_state osnovni60
let s100 = initialize_state osnovni100





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
(* let zapisi_minimalne (state)=
   let mini = ref [] in
   if state.vrstice.min_dolzina = state.minimalen then 
     mini := (state.vrstice.min_indeksi, Vrstica)::!mini;
   if state.stolpci.min_dolzina = state.minimalen then 
    mini := (state.stolpci.min_indeksi, Stolpec)::!mini;
   if state.boxi.min_dolzina = state.minimalen then
    mini := (state.boxi.min_indeksi, Box)::!mini; 
  state.minimalni <- !mini *)

(* let daj_na_izracun objekt = 
  let rec daj_na_izracun_aux acc = function
      | [] -> List.flatten acc
      | x :: xs-> daj_na_izracun_aux ((objekt.indeksi_praznih.(x))::acc) xs 
in daj_na_izracun_aux [] objekt.min_indeksi *)

let state2 = {problem =osnovni2; current_grid=osnovni2.initial_grid; vrstice= cal_empty Vrstica osnovni2.initial_grid ; stolpci= cal_empty Stolpec osnovni2.initial_grid; boxi= cal_empty Box osnovni2.initial_grid ; za_resevanje=[||]}

let state1 = {problem=osnovni; current_grid=osnovni.initial_grid;  vrstice=cal_empty Vrstica osnovni.initial_grid ; stolpci= cal_empty Stolpec osnovni.initial_grid; boxi= cal_empty Box osnovni.initial_grid ; za_resevanje=[||] }

let prazne_moznosti sez_noneov (state) = 
  let rec prazne_moznosti_aux acc = function
      | [] ->  Array.of_list acc
      | (x,y) :: xs -> let skupni = presek3eh (state.vrstice.manjkajoci.(x)) (state.stolpci.manjkajoci.(y)) (state.boxi.manjkajoci.((izracun_boxa_iz_koordinat x y))) in 
                        let podatekxy = {loc=(x,y); possible= skupni}
                        in 
                        prazne_moznosti_aux (podatekxy::acc) (xs)
in prazne_moznosti_aux [] sez_noneov

(*spremeni,da vemo se indekse katerih unija je to*)

let za_vsak_min_objekt_izracunaj indeksi (objekt) (state)= 
  let rec daj_na_izracun_aux acc = function
     | [] -> Array.of_list acc
     | x::xs ->let rez = prazne_moznosti (objekt.indeksi_praznih.(x)) (state) in
                daj_na_izracun_aux (rez::acc) (xs)
in daj_na_izracun_aux [] indeksi 

let za_vse_izracunaj (state) = 
  let vsi_od_vrstic = za_vsak_min_objekt_izracunaj [0;1;2;3;4;5;6;7;8] (state.vrstice) (state) in
  let vsi_od_stolpcev = za_vsak_min_objekt_izracunaj [0;1;2;3;4;5;6;7;8] (state.stolpci) (state) in
  let  vsi_od_boxov = za_vsak_min_objekt_izracunaj [0;1;2;3;4;5;6;7;8] (state.boxi) (state) in  
  let vsi = [|vsi_od_vrstic;vsi_od_stolpcev;vsi_od_boxov|] in 
  state.za_resevanje <- (vsi)




  let print_list lst =
    List.iter (fun { loc = (f1, f2); possible = f3 } -> Printf.printf "field1: (%d, %d), field2: %s\n" f1 f2 (String.concat ", " (List.map string_of_int f3))) lst;;
  


(* let za_vse_minimalne (state) = 
  let rec za_vse_minimalne_aux acc = function
      | [] -> state.za_resevanje <- (Array.of_list acc)
      | (x,y)::xs -> match y with 
                      | Vrstica -> let vsi_od_vrstic = za_vsak_min_objekt_izracunaj (x) (state.vrstice) (state) in
                                    za_vse_minimalne_aux (vsi_od_vrstic::acc) xs
                      | Stolpec -> let vsi_od_stolpcev = za_vsak_min_objekt_izracunaj (x) (state.stolpci) (state) in
                      za_vse_minimalne_aux (vsi_od_stolpcev::acc) xs
                      | Box -> let vsi_od_boxov = za_vsak_min_objekt_izracunaj (x) (state.boxi)  (state) in
                      za_vse_minimalne_aux (vsi_od_boxov::acc) xs
in za_vse_minimalne_aux [] state.minimalni *)
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

(*possible list je urejen narascajoce*)
(*primerja glavi obeh listov, ce sta enaki da glavo na acc in gre pri obeh gledat naslednjega ce pa je npr. [5,7] [7] ->
  5 da na acc s 7 pocaka ce bo naslednja glava njej enaka *)
let unija_2eh_listov i j = 
  let rec unija_dveh_rec acc = function
      | ([],[]) -> acc
      | ([],y) -> y @ acc
      | (x, []) -> x @ acc
      | (x::xs, y::ys) -> if x = y then  unija_dveh_rec (x :: acc) (xs,ys) 
                           else  
                            if x < y then unija_dveh_rec (x:: acc) (xs, y::ys)
                             else 
                              unija_dveh_rec (y::acc) (x::xs, ys)
in unija_dveh_rec [] (i,j)
(*Zaenkrat vrne padajoce*)         
 
(*najdaljsa unija je dolzine 1 manj od vseh praznih (brezveze cekirart unijo vseh sej ves da morjo prit sm)*)
let boxic = prazne_moznosti (state2.boxi.indeksi_praznih.(2)) state2 








let k = [[1;5;7]; [1;5]; [1;7]]

let odstrani_iz_sez availabalsi index elementi_iz_unije = 
  let rec odstrani_iz_sez_rec acc = function
      | ([],[]) -> availabalsi.(index).possible <- (List.rev acc)
      | ([],y) -> availabalsi.(index).possible <- (List.rev acc)
      | (x, []) -> availabalsi.(index).possible <- (List.rev x @ acc)
      | (x::xs, y::ys) -> if x = y then  odstrani_iz_sez_rec (acc) (xs,ys) 
                            else  
                              if x < y then odstrani_iz_sez_rec (x::(acc)) (xs, y::ys)
                                else 
                                   odstrani_iz_sez_rec (acc) (x::xs, ys)
in odstrani_iz_sez_rec [] ((availabalsi.(index).possible), elementi_iz_unije)




let odstrani_iz_moznosti (availabalsi : available array) (za_odstranit, indeksi) = 
    for i=0 to Array.length (availabalsi) -1  do 
      if List.mem i indeksi= false then 
        odstrani_iz_sez (availabalsi) (i) (za_odstranit);
    done


let ali_je_dobra_unija arrunija indeksi state i j= 
  let prava_unija unija =
    let zacetni = ref [] in
    for k = 0 to 8 do 
      if unija.(k) <> None then zacetni := (k+1):: !zacetni;
    done;
    (!zacetni) 
  in 
  let rez = prava_unija arrunija in
  if List.length rez = List.length indeksi then  odstrani_iz_moznosti (state.za_resevanje.(i).(j)) (List.rev rez, indeksi)


let unija2 possiblesi indeksi  (state) (i) (j)= 
  let vsi= Array.make 9 None in 
  let rec ali_vsebuje = function
      | [] ->  ali_je_dobra_unija (vsi) (indeksi) (state) (i) (j)
      | x :: xs -> if vsi.(x-1) = None then vsi.(x-1) <- Some x;
                    ali_vsebuje xs
in ali_vsebuje (List.flatten possiblesi) 




let loci_indekse possible state i j = 
  let rec loci_indekse_aux acc_possiblesi acc_indeksi = function
     | [] ->  unija2 (acc_possiblesi) (acc_indeksi) (state) (i) (j)
     | (p, ind) :: xs -> loci_indekse_aux (p::acc_possiblesi) (ind::acc_indeksi) (xs)
in loci_indekse_aux [] [] possible
     
(* loci_indekse [([1;5;7],0);([1;5],1);([1;7],2)];;
- : (int list * int list) option = Some ([7; 5; 1], [2; 1; 0])  *)
(*some x -> treba obrnit*)
(* let vmesna = function 
    | Some x -> odstrani_iz_moznosti "vrstica" *)



(* # odstrani_iz_sez boxic 1 [1;5;7] ;;
- : unit = ()
# boxic ;;
- : available array =
[|{loc = (0, 7); possible = [1; 5; 7]}; {loc = (0, 8); possible = [6]}; 
  {loc = (1, 6); possible = [1; 5]}; {loc = (2, 7); possible = [1; 7]}|]
# *)






(*samo za vsako vrstico box in stolpec ki je min mors to narest*)
(*izmed n elementov (mnozice moznih za vsako prazno celico v objektu) izbiramo k elementov( za unijo) gre po formuli :
   (n C k)= ((n-1)C(k-1)) + ((n-1)C(k))*)

let vse_podmnozice possiblesi state i j=
  let rec vse_k_podmnozice k list =
      if k <= 0 then [[]]
      else 
        match list with
          | [] -> []
          | x :: xs ->
              let z_x_om = List.map (fun l -> x :: l) (vse_k_podmnozice (k - 1) xs) in
              let brez_x_a = vse_k_podmnozice k xs in
              z_x_om @ brez_x_a
  in 
  let dolzina = List.length possiblesi in  
  let vsi = ref [] in
  for i = 2 to dolzina - 1 do 
    vsi := (vse_k_podmnozice i possiblesi )::(!vsi)
  done;
  (* (List.flatten !vsi) *)
  let poslji_naprej = List.flatten !vsi in 
  List.iter (fun x -> loci_indekse (x) (state) (i) (j)) poslji_naprej

(*mora dobit possiblese ([1,2,3],0) (manjkajoci, indeksi)*)

(* 
let vse_podmnozice sez state i j = 
  state.delej <- sez *)

  (* # zapisi_minimalne state2 ;;
  - : unit = ()
  # za_vse_minimalne state2 ;;
  - : unit = ()
  # pridobi_objekt state2 ;; *)




(* 
let branch_state *)
(*PREGLEDA vse possiblese ce je moznost samo ena jo resi ce je moznost nobena je napaka ce ne si *)
(*za pregled possiblesov ki so narascajoce urejeni*)
let swap a i j = 
  let v = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- v 

let index_min a lower upper = 
  let im = ref lower in
  for i = lower to upper do 
     if List.length a.(i).possible < List.length a.(!im).possible then im := i (*ker ma prirejanje tip unit ne rabmo narest elsa*)
  done;
  !im

let selection_sort_array av = 
  let najprej =  Array.map (fun x -> Array.concat (Array.to_list (x))) av in
  let a= najprej|> Array.to_list |> Array.concat in 
  let index_end = Array.length a - 1 in 
  for boundary_sorted = 0 to index_end do 
      let i = index_min a boundary_sorted index_end in
      swap a i boundary_sorted (* najmanjsi element ki smo ga nasli zamenjamo na tisto mesto, kjer je trenutno nasa meja*)
  done;
  a



let odstrani_enake_in_uredi_enojne urejen_array = 
  let rec daj_na_branch acc1 acc2 loci = function
      | [] -> Some(acc1, List.rev acc2)
      | x :: xs -> if List.mem x.loc loci then 
                      daj_na_branch acc1 acc2 loci xs else
                     if List.length x.possible = 1 then 
                      daj_na_branch (x::acc1) (acc2) (x.loc :: loci) xs
                      else 
                        if List.length x.possible = 0 then None 
                        else
                        daj_na_branch (acc1) (x::acc2) (x.loc :: loci) xs      
in daj_na_branch [] [] [] (Array.to_list urejen_array)





let preglej_unije_podmnozic_minov (state) (*(arr : available array array array)*) = 
za_vse_izracunaj state;
let arr = state.za_resevanje in 
for i= 0 to Array.length arr - 1  do 
  for j=0 to Array.length arr.(i) - 1  do
    let izlusci_possiblese sez = 
      let rec izlusci_possiblese_aux index acc = function 
          | [] -> vse_podmnozice acc (state) (i) (j)
          | x ::xs -> izlusci_possiblese_aux  (index+1) ((x.possible, index ):: acc) xs
      in izlusci_possiblese_aux 0 [] (Array.to_list sez)
    in izlusci_possiblese arr.(i).(j)
  done;
done

(* let preglej_vse_unije state =  *)
  


let odstrani av_element arra novi_element = 
  let rec odstrani_aux nova_arr = function
    | [] -> Array.of_list (novi_element :: nova_arr )
    | x ::xs -> if x.loc = av_element.loc then 
                  odstrani_aux nova_arr xs
                else 
                  odstrani_aux (x::nova_arr) xs
in odstrani_aux [] (Array.to_list arra)
           


 



  let rec is_consecutive lst =
    match List.sort compare lst with
    | [] | [_] -> true
    | a :: b :: tl -> if b = a + 1 then is_consecutive (b :: tl) else false



   type solution = int grid


  type response = Solved of solution | Unsolved of statenew | Fail of statenew

  let je_pravilna (problem) (vrstica) (indexx) = 
    let rec je_pravilna_aux indexy = function
        | [] -> true
        | x :: xs -> match x with 
                     | None -> je_pravilna_aux (indexy + 1) xs
                     | Some b -> if b = vrstica.(indexy) then je_pravilna_aux (indexy +1) (xs) else false
  in je_pravilna_aux 0 (Array.to_list (problem.initial_grid.(indexx)))

  let is_valid_solution problem solution = 
    let compare_to_problem (rows: 'a array list) = 
      let rec compare_to_problem_aux index = function
          | [] -> true
          | x::xs -> if je_pravilna (problem) (x) (index) then compare_to_problem_aux (index+1) (xs) else false
      in compare_to_problem_aux 0 rows
    in
    let rec preveri vrstice stolpci boxi = 
      match (vrstice, stolpci,boxi) with
      | ([], [],[]) -> compare_to_problem (rows solution)
      | ([], _,_) -> false
      | (_, [],_) -> false
      | (_,_,[]) -> false
      | (x :: xs, y:: ys, z::zs) -> if (is_consecutive (Array.to_list (x))) && (is_consecutive (Array.to_list (y))) && (is_consecutive (Array.to_list (z))) then preveri (xs) (ys) (zs) else false
  in preveri (rows solution) (columns solution) (boxes solution)


  let validate_state (state ) : response =
    let unsolved =
      Array.exists (Array.exists Option.is_none) state.current_grid
    in
    if unsolved then Unsolved state
    else
      (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
      let solution = map_grid Option.get state.current_grid in
      if is_valid_solution state.problem solution then Solved solution
      else Fail state




let print_state (state) : unit =
  print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid


let preglej_nov arr =
  let lst = Array.to_list arr in
  let rec collect_options acc = function
    | [] -> acc
    | None :: tl -> collect_options acc tl
    | Some x :: tl -> collect_options (x :: acc) tl
  in
  let sorted = List.sort compare (collect_options [] lst) in
  let rec check_consecutive = function
    | [] | [_] -> true
    | x :: y :: tl -> if x  = y then false else check_consecutive (y :: tl)
  in
  check_consecutive sorted

  let preglej_celo grid = 
    let rec preveri vrstice stolpci boxi = 
      match (vrstice, stolpci, boxi) with
      | ([], [], []) -> true
      | ([], _,_) -> false
      | (_, [],_) -> false
      | (_,_,[]) -> false
      | (x :: xs, y:: ys, z:: zs) -> if (preglej_nov (x )) && (preglej_nov (y))&&(preglej_nov (z))  then preveri (xs) (ys) (zs) else false
  in preveri (rows grid) (columns grid) (boxes grid)


  let is_valid_move grid x y el =
    let vse_vrstice =Array.of_list (rows grid) in 
    let row_ok = Array.for_all (fun elt -> elt <> Some el) vse_vrstice.(x) in
    let vsi_stolpci =Array.of_list (columns grid )in
    let col_ok = Array.for_all (fun elt -> elt <> Some el) vsi_stolpci.(y) in
    let vsi_boxi =Array.of_list (boxes grid) in 
    let box_ok = Array.for_all (fun elt -> elt <> Some el) vsi_boxi.(izracun_boxa_iz_koordinat(x) (y))
    in
    row_ok && col_ok && box_ok
  

let are_singles_valid lst_singles grid = 
  let rec are_singles_valid_aux = function
    | [] -> true
    | s:: xs -> let x,y = s.loc in 
                let single = List.nth s.possible 0 in 
                if not (is_valid_move (grid) (x) (y) (single)) then false
                else  are_singles_valid_aux xs 
in are_singles_valid_aux lst_singles



let nova_grid i j element grid = 
    let new_grid = copy_grid grid in
    new_grid.(i).(j) <- Some element; 
    new_grid 



let izpolni_enojce lst_enojci grid = 
  let nov_grid =copy_grid grid in
  let rec izpolni_enojce_aux = function
      | [] -> nov_grid
      | s:: xs -> let x,y = s.loc in 
                  let enojec = List.nth (s.possible) 0 in 
                  nov_grid.(x).(y) <- Some enojec;
                  izpolni_enojce_aux xs
  in izpolni_enojce_aux lst_enojci


  let rec process_data data =
    match data with
    | Some x -> Array.map (Array.map (fun y -> y + 1)) x
    | None -> [||]

let branch_state (state) = 
  if state.za_resevanje =[||] then preglej_unije_podmnozic_minov state;
  print_state(state);
  print_newline ();
  let update_av = selection_sort_array state.za_resevanje in 
  let urejeni = odstrani_enake_in_uredi_enojne (update_av) in 
  match urejeni with 
  | None -> None 
  | Some([],[]) -> None
  | Some([], drugi) ->  let izbira = List.nth drugi 0 in 
                        (*ze prej bi prislo do napake ce bi bila prazni possiblesi*)
                       let element = List.nth (izbira.possible) 0 in
                       let x,y = izbira.loc in
                       let izpolnjena_cell = nova_grid (x) (y) (element) (state.current_grid) in 
                       let box = boxes izpolnjena_cell in 
                       let boxij = (Array.of_list (box)).(izracun_boxa_iz_koordinat x y) in
                       if preglej_nov(izpolnjena_cell.(x)) && preglej_nov(izpolnjena_cell.(y)) && preglej_nov(boxij) then 
                          let ostali = List.filter (fun x -> x <> element) izbira.possible in 
                          let novi_av = {loc=(x,y); possible=ostali} in 
                          let za_resevanje = odstrani (izbira) (update_av) (novi_av) in 
                          let novi_state_za_resevanje = [|[|za_resevanje|]|] in
                          let state1={problem=state.problem; current_grid=izpolnjena_cell; vrstice=cal_empty Vrstica izpolnjena_cell; stolpci= cal_empty Stolpec izpolnjena_cell; boxi=cal_empty Box izpolnjena_cell ; za_resevanje=[||] } in
                          let state2 = {problem=state.problem; current_grid=copy_grid state.current_grid; vrstice=state.vrstice; stolpci=state.stolpci; boxi=state.boxi;  za_resevanje=novi_state_za_resevanje} in
                          Some(state1, Some state2)
                       else 
                        None
  | Some(enojci,_) ->   let nova = izpolni_enojce (enojci) (state.current_grid) in 
                        if preglej_celo nova then
                            let state_od_enojcev = {problem=state.problem; current_grid=nova; vrstice=cal_empty Vrstica nova; stolpci= cal_empty Stolpec nova; boxi=cal_empty Box nova ;za_resevanje=[||] } in
                            Some(state_od_enojcev,None)
                        else 
                          None
                       (*mora se odlocit med drugimi possiblesi v listu*)


let pridobi_rez  = function
    | None -> [||]
    | Some(st, None) -> [|st|]
    | Some (st1, Some st2) -> [|st1; st2|]
     

let resitev1 = branch_state state1
let resitev2 = branch_state state2 
let solu1 = pridobi_rez resitev1 
let solu2 = pridobi_rez resitev2 


let depth = ref 0

 (* pogledamo, če trenutno stanje vodi do rešitve *)
 let rec solve_state (state) =
   Printf.printf "Solving state %d\n" !depth;
    print_newline ();
    incr depth;
   (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)
   (* TODO: na tej točki je stanje smiselno počistiti in zožiti možne rešitve *) 
   match validate_state state with
   | Solved solution ->
       (* če smo našli rešitev, končamo *)
       Some solution
   | Fail fail ->
       (* prav tako končamo, če smo odkrili, da rešitev ni *)
       None
   | Unsolved state' ->
       (* če še nismo končali, raziščemo stanje, v katerem smo končali *)
       explore_state state'
 
 and explore_state (state) =
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
   | Some (st1, None) ->
       match solve_state st1 with 
         | Some solution -> Some solution 
         | None -> None




