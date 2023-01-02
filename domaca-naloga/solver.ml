type available = { loc : int * int; possible : int list }

(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)
type state = { problem : Model.problem; current_grid : int option Model.grid; empty_rows : int list ; available : available option}

let print_state (state : state) : unit =
  Model.print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of Model.solution | Unsolved of state | Fail of state

let initialize_state (problem : Model.problem) : state =
  { current_grid = Model.copy_grid problem.initial_grid; problem ; empty_rows = Model.cal_empty_rows (problem.initial_grid) ; available=None }

let validate_state (state : state) : response =
  let unsolved =
    Array.exists (Array.exists Option.is_none) state.current_grid
  in
  if unsolved then Unsolved state
  else
    (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
    let solution = Model.map_grid Option.get state.current_grid in
    if Model.is_valid_solution state.problem solution then Solved solution
    else Fail state

type v_vrstici = {indeksi_none_stolpci: int list ; indeks_vrstice: int ; zasedeni: int list}
(*Prikaze pozicijo praznih celic in seznam stevilskih elementov, ki so  v vrstici*)
let ze_v_vrstici (state : state) (index : int ) = 
  let rec ze_v_vrstici_aux acc ind_druge indexi_noneov = function
      | [] -> {indeksi_none_stolpci = indexi_noneov; indeks_vrstice = index; zasedeni = acc }
      | (Some x ) :: xs -> ze_v_vrstici_aux (x:: acc) (ind_druge + 1) (indexi_noneov) (xs)
      | None :: xs -> ze_v_vrstici_aux (acc) (ind_druge + 1) (ind_druge :: indexi_noneov) (xs)
in ze_v_vrstici_aux [] 0 [] (Array.to_list (Model.get_row (state.current_grid) index ) )
    
type v_stolpcu = {indeksi_none_vrstice: int list ; indeks_stolpca: int ; zasedeni: int list}
(*Prikaze pozicijo praznih celic in seznam stevilskih elementov, ki so  v stolpcu *) 
let ze_v_stolpcu (state : state) (index : int ) = 
  let rec ze_v_stolpcu_aux acc ind_druge indexi_noneov = function
    | [] -> {indeksi_none_vrstice = indexi_noneov; indeks_stolpca = index; zasedeni = acc }
    | (Some x ) :: xs -> ze_v_stolpcu_aux (x:: acc) (ind_druge + 1) (indexi_noneov) (xs)
    | None :: xs -> ze_v_stolpcu_aux (acc) (ind_druge + 1) (ind_druge :: indexi_noneov) (xs)
in ze_v_stolpcu_aux [] 0 [] (Array.to_list (Model.get_column (state.current_grid) index ) )
    
(*prvo stanje, drugo stanje*)
  (* TODO: Pripravite funkcijo, ki v trenutnem stanju poišče hipotezo, glede katere
     se je treba odločiti. Če ta obstaja, stanje razveji na dve stanji:
     v prvem predpostavi, da hipoteza velja, v drugem pa ravno obratno.
     Če bo vaš algoritem najprej poizkusil prvo možnost, vam morda pri drugi
     za začetek ni treba zapravljati preveč časa, saj ne bo nujno prišla v poštev. *)

(*zaenkrat je funkcija prirejena za resevanje sudokuja obicajni-1 kjer je v vrstici manjkajočih največ 2 *)
let branch_state (state : state) : (state * state option) option = 
  (*Pogledamo prvo prazno celico v prvi nepolni vrstici(vrstica ki vsebuje prazne celice)
in poiscemo njene manjkajoce elemente, ki jih zapisemo v state.available*)
  let nepolne_vrstice = Array.of_list (state.empty_rows)
  in let prva_nepolna_vrstica = nepolne_vrstice.(0) 
  in let podatki_o_vrstici =  ze_v_vrstici (state) (prva_nepolna_vrstica)
  in let noni = (Array.of_list (podatki_o_vrstici.indeksi_none_stolpci))
  in let prvi_none = noni.(0)
  in match Model.manjkajoci (podatki_o_vrstici.zasedeni)with
      | None -> None
      | Some lst->let array_moznih = Array.of_list lst in 
                  if Array.length array_moznih <> 1 then 
                    (*ce sta mozna dva bo najprej poskusil s prvim ,  ce ne uspe se z drugim -> *)
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
(*prazno celico v gridu nastavi na možno rešitev*)
let nova_grid i j element grid = 
  Array.init 9 (fun vrstica -> Array.init 9 (fun st -> if st = j && i = vrstica then (Some element) else grid.(vrstica).(st)))    
                    
let izpolni_grid (state : state) =
  (*ce available ni None je v available.loc zapisan mesto prazne celice v available.possible pa ena možna rešitev*)
    match state.available with 
      | None -> state
      | Some av -> let vrstica, stolpec = av.loc in 
                    let mozni = Array.of_list (av.possible) in
                    let mozni_element = mozni.(0) in
                    let new_grid = nova_grid (vrstica) (stolpec) (mozni_element) (state.current_grid)
                    in 
                    {problem=state.problem; current_grid=new_grid ; empty_rows= Model.cal_empty_rows (new_grid); available=None}

(* pogledamo, če trenutno stanje vodi do rešitve *)
let rec solve_state (state : state) =
  (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)
  (* TODO: na tej točki je stanje smiselno počistiti in zožiti možne rešitve *)
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

let solve_problem (problem : Model.problem) =
  problem |> initialize_state |> solve_state
