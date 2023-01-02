(* Pomožni tip, ki predstavlja mrežo *)
type 'a grid = 'a Array.t Array.t

(* Funkcije za prikaz mreže.
   Te definiramo najprej, da si lahko z njimi pomagamo pri iskanju napak. *)

(* Razbije seznam [lst] v seznam seznamov dolžine [size] *)
let chunkify size lst =
  let rec aux chunk chunks n lst =
    match (n, lst) with
    | _, [] when chunk = [] -> List.rev chunks
    | _, [] -> List.rev (List.rev chunk :: chunks)
    | 0, _ :: _ -> aux [] (List.rev chunk :: chunks) size lst
    | _, x :: xs -> aux (x :: chunk) chunks (n - 1) xs
  in
  aux [] [] size lst

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

(* Funkcije za dostopanje do elementov mreže *)

let get_row (grid : 'a grid) (row_ind : int) = 
  Array.init 9 (fun stolpec -> grid.(row_ind).(stolpec))

let rows grid = List.init 9 (get_row grid)

let get_column (grid : 'a grid) (col_ind : int) =
  Array.init 9 (fun row_ind -> grid.(row_ind).(col_ind))

let columns grid = List.init 9 (get_column grid)

let mapi_grid f grid = 
  Array.init 3 (fun vrstica -> Array.map f grid.(vrstica))

let get_box (grid : 'a grid) (box_ind : int) = 
  (*vsak box ima elemente iz treh vrstic  prsva vrstica ima zacetni indeks*)
  let zacetni = box_ind - (box_ind mod 3)
  in
  let row_blocks grid = (*ustrezne vrstice razdelimo na sezname dolzine 3*)
  Array.init 3 (fun vrstica -> grid.(zacetni + vrstica)|> Array.to_list |> chunkify 3 |> Array.of_list)
  in 
  let razdeljene_vrstice = row_blocks grid 
  in 
  let deli_vrstice_v_array_obliki = mapi_grid (Array.of_list) razdeljene_vrstice 
  in (*kater del od vrstice vzame ustreza box_ind mod 3*)
  Array.init 3 (fun vrstica -> deli_vrstice_v_array_obliki.(vrstica).(box_ind mod 3))

let boxes grid = List.init 9 (get_box grid)

(* Funkcije za ustvarjanje novih mrež *)


let map_grid (f : 'a -> 'b) (grid : 'a grid) : 'b grid = 
  Array.init 9 (fun vrstica -> Array.map f grid.(vrstica))

let copy_grid (grid : 'a grid) : 'a grid = map_grid (fun x -> x) grid

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

(* Model za vhodne probleme *)

type problem = { initial_grid : int option grid } 

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

(*vrne int list z indeksi vrstic, ki imajo vsaj eno celico neizpolnjeno*)
let cal_empty_rows grid = 
  let rec empty_rows_aux acc index (*dobis list of arrays*) = function
      | [] -> List.rev acc
      | x :: xs -> if (Array.exists (Option.is_none) x ) then (empty_rows_aux (index :: acc) (index + 1) xs )
                    else (empty_rows_aux (acc) (index + 1) xs)
in empty_rows_aux [] 0 (rows grid) 

(*Pomozn funckije za selection_sort*)
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
(*Poisce katera stevila med 1 in 9 manjkajo v seznamu pridobljen iz elementov v vrstici/stolpcu/boxu*)
(*+ preveri, ce sta kaksna dva elementa v seznamu enaka*)
let manjkajoci (list: int list) = 
  let rec manjkajoci_aux acc = function
      | [] -> Some acc
      | [x] -> Some acc
      | x :: y :: xs -> if x + 1 = y then manjkajoci_aux (acc) (y :: xs) else 
        if x = y then None
        else manjkajoci_aux ((x+1) :: acc) ((x+1)::y::xs)
in manjkajoci_aux [] (selection_sort (0 ::(10) :: list))


(* Model za izhodne rešitve *)

type solution = int grid

let print_solution solution = print_grid (fun i -> string_of_int i) solution

(*preveri vse vrstice in stolpce, ce so ustrezni bo za vsako vrstico/stolpce funkcija manjkajoci
   vrnila Some [] saj nobeden od elementov od 1 do 9 ne manjka in nobeden se ne ponovi*)
let is_valid_solution problem solution = 
  let rec preveri vrstice stolpci = 
    match (vrstice, stolpci) with
    | ([], []) -> true
    | ([], _) -> false
    | (_, []) -> false
    | (x :: xs, y:: ys) -> if (manjkajoci (Array.to_list (x ))= Some []) && (manjkajoci (Array.to_list (y)) = Some []) then preveri (xs) (ys) else false
in preveri (rows solution) (columns solution)
