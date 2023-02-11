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
  let razdeljena = Array.init 3 (fun vrstica -> deli_vrstice_v_array_obliki.(vrstica).(box_ind mod 3))
  in Array.concat ( Array.to_list razdeljena)

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

let izracun_boxa_iz_koordinat x y = 
  x - x mod 3 + y/3


(*Poisce katera stevila med 1 in 9 manjkajo v seznamu pridobljen iz elementov v vrstici/stolpcu/boxu*)
(*+ check, ce sta kaksna dva elementa v seznamu enaka*)
(* Model za izhodne rešitve *)

type solution = int grid

let print_solution solution = print_grid (fun i -> string_of_int i) solution


let rec are_consecutive lst =
  match List.sort compare lst with
  | [] | [_] -> true
  | x :: y :: xs -> if y = x + 1 then are_consecutive (y :: xs) else false

(*preveri ujemanje vrstice od solution s problemom*)
let check_row (problem) (vrstica) (indexx) = 
  let rec check_row_aux indexy = function
      | [] -> true
      | x :: xs -> match x with 
                    | None -> check_row_aux (indexy + 1) xs
                    | Some b -> if b = vrstica.(indexy) then check_row_aux (indexy +1) (xs) else false
in check_row_aux 0 (Array.to_list (problem.initial_grid.(indexx)))

let is_valid_solution problem solution = 
  let compare_to_problem (rows: 'a array list) = 
    let rec compare_to_problem_aux index = function
        | [] -> true
        | x::xs -> if check_row (problem) (x) (index) then compare_to_problem_aux (index+1) (xs) else false
    in compare_to_problem_aux 0 rows
  in
  let rec check vrstice stolpci boxi = 
    match (vrstice, stolpci,boxi) with
    | ([], [],[]) -> compare_to_problem (rows solution)
    | ([], _,_) -> false
    | (_, [],_) -> false
    | (_,_,[]) -> false
    | (x :: xs, y:: ys, z::zs) -> if (are_consecutive (Array.to_list (x))) && (are_consecutive (Array.to_list (y))) && (are_consecutive (Array.to_list (z))) then check (xs) (ys) (zs) else false
  in check (rows solution) (columns solution) (boxes solution)


type available = { loc : int * int; mutable possible : int list }

type objekt = Vrstica | Stolpec | Box 

type lastnosti_objekta = { indeksi_praznih : (int * int) list array ; vrsta : objekt ; manjkajoci : int list array }
  
(*vsakic na zacetku izracuna manjkajoce(elementi v list niso enaki)*)
let manjkajoci (list: int list) = 
  let rec manjkajoci_aux acc = function
      | [] -> acc
      | [x] -> acc
      | x :: y :: xs -> if x + 1 = y then manjkajoci_aux (acc) (y :: xs)
        else manjkajoci_aux ((x+1) :: acc) ((x+1)::y::xs)
  in  
  let za_uredit = 0::10::list in
  manjkajoci_aux [] (List.sort compare za_uredit)
   
 
   (*Naslednje funkcije za vrstice/stolpce/boxe v gridu izracunajo:indekse_praznih in manjkajoce stevilke v posz vrstici/boxu/stolpcu*)
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
  let rec cal_empty_aux acc_prazni acc_manjkajoci  index (*dobis list of arrays*) = function
      | [] -> {indeksi_praznih = Array.of_list (List.rev acc_prazni); vrsta=vrsta_objekta; manjkajoci= Array.of_list (List.rev acc_manjkajoci)}
      | x :: xs -> if (Array.exists (Option.is_none) x ) then 
                    let prazni, manjkajoci = v_objektu (index) (x) (vrsta_objekta) in
                        cal_empty_aux (prazni:: acc_prazni) (manjkajoci :: acc_manjkajoci) (index + 1) xs 
                    else 
                        cal_empty_aux ([]::acc_prazni) ([]::acc_manjkajoci) (index + 1) xs
  in 
    match vrsta_objekta with 
        | Vrstica -> cal_empty_aux [] []  0 (rows grid) 
        | Stolpec -> cal_empty_aux [] []  0 (columns grid)
        | Box -> cal_empty_aux [] []  0 (boxes grid)
  
 
 type state = { problem : problem; current_grid : int option grid ; vrstice : lastnosti_objekta ; stolpci : lastnosti_objekta ; boxi : lastnosti_objekta; mutable za_resevanje: available array array array} 
 
 let print_state (state : state) : unit =
   print_grid
     (function None -> "?" | Some digit -> string_of_int digit)
     state.current_grid
 
 type response = Solved of solution | Unsolved of state | Fail of state
 
 let initialize_state (problem : problem) : state =
   { current_grid = copy_grid problem.initial_grid; problem=problem ; vrstice=cal_empty Vrstica problem.initial_grid; stolpci= cal_empty Stolpec problem.initial_grid; boxi=cal_empty Box problem.initial_grid ;za_resevanje=[||] }
 

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
 
(*izracuna mozne za prazno celico ki so presek moznih za vrstico, stolpca in boxa v kateri je*)
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
  

 (*za vsako vrstico,stolpec in box zapise available array : available: loc--> koordinate prazne celice possible:
    (presek manjkajocih v vrstici/stolpcu/boxu v katerem leži)*)
let prazne_moznosti sez_noneov (state) = 
  let rec prazne_moznosti_aux acc = function
      | [] ->  Array.of_list acc
      | (x,y) :: xs -> let skupni = presek3eh (state.vrstice.manjkajoci.(x)) (state.stolpci.manjkajoci.(y))
      (state.boxi.manjkajoci.((izracun_boxa_iz_koordinat x y))) in 
                        let podatekxy = {loc=(x,y); possible= skupni}
                        in 
                        prazne_moznosti_aux (podatekxy::acc) (xs)
  in prazne_moznosti_aux [] sez_noneov


let za_vsak_objekt_izracunaj indeksi (objekt) (state)= 
  let rec daj_na_izracun_aux acc = function
      | [] -> Array.of_list acc
      | x::xs ->let rez = prazne_moznosti (objekt.indeksi_praznih.(x)) (state) in
                daj_na_izracun_aux (rez::acc) (xs)
  in daj_na_izracun_aux [] indeksi 

let za_vse_izracunaj (state) = 
  let vsi_od_vrstic = za_vsak_objekt_izracunaj [0;1;2;3;4;5;6;7;8] (state.vrstice) (state) in
  let vsi_od_stolpcev = za_vsak_objekt_izracunaj [0;1;2;3;4;5;6;7;8] (state.stolpci) (state) in
  let  vsi_od_boxov = za_vsak_objekt_izracunaj [0;1;2;3;4;5;6;7;8] (state.boxi) (state) in  
  let vsi = [|vsi_od_vrstic;vsi_od_stolpcev;vsi_od_boxov|] in 
  state.za_resevanje <- (vsi)
   
 
  (*Naslednje funkcije izracunajo vse mozne unije v posz vrstici/stolpcu/boxu:
      npr. so v vrstici prazni elementi z av.possible : [1;5;7]; [1;5];[1;7] [1;5;6;7]
      Ce naredimo unijo :  [1;5;7]in[1;5]in[1;7] = [1;5;7] (unija moznih za ta 3 mesta so 3 ->te stevilke morajo biti na teh treh mestih zato jih lahko odstranimo iz 
      seznamov moznih za druga prazna mesta(dobra unija) -> v zadnjem samo : [6]) *)

  (*elemente iz dobre unije odstrani  iz moznosti za ostala prazna mesta *)
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

(*check ce je unija enako dolga kot st. seznamov possiblesov(=st praznih celic) iz katerih je sestavljena*)
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

(*naredi unijo seznamov possiblesov*)
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


(*vse mozne unije possiblesov praznih celic v vrstici/stolpcu/boxu*)
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
  let poslji_naprej = List.flatten !vsi in 
  List.iter (fun x -> loci_indekse (x) (state) (i) (j)) poslji_naprej
   

(*state.za_resevanje : [|objekti|] npr. vrstice:[|[|posz-vrstica|]|] za posz_vrstico pogledamo unije*)
let preglej_unije (state) (*(arr : available array array array)*) = 
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
 
 (*Iz state.za_resevanje naredi en av(available) array in ga uredi glede na dolzino av.possible
    (da bodo tiste z manj moznimi na zacetku)*)
let swap a i j = 
  let v = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- v 

let index_min a lower upper = 
  let im = ref lower in
  for i = lower to upper do 
      if List.length a.(i).possible < List.length a.(!im).possible then im := i 
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
     

(*prazno celico v gridu nastavi na možno rešitev*)
let nova_grid i j element grid = 
  let new_grid = copy_grid grid in
  new_grid.(i).(j) <- Some element; 
  new_grid 

     
(*reši enojce*)
let izpolni_enojce lst_enojci grid = 
    let nova_grid = copy_grid grid in
    let rec izpolni_enojce_aux = function
        | [] -> nova_grid
        | s:: xs -> let x,y = s.loc in 
                    let enojec = List.nth (s.possible) 0 in 
                      nova_grid.(x).(y) <- Some enojec;
                      izpolni_enojce_aux xs
    in izpolni_enojce_aux lst_enojci
  

(*pregleda nov dobljen gridd*)
let check_new arr =
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

let check_all grid = 
  let rec check vrstice stolpci boxi = 
    match (vrstice, stolpci, boxi) with
    | ([], [], []) -> true
    | ([], _,_) -> false
    | (_, [],_) -> false
    | (_,_,[]) -> false
    | (x :: xs, y:: ys, z:: zs) -> if (check_new (x )) && (check_new (y))&&(check_new (z))  then check (xs) (ys) (zs) else false
  in check (rows grid) (columns grid) (boxes grid)


(* posodobi available array , ki bo sestavljal state2.za_resevanje *)
let odstrani av_element stari_av novi_element = 
  let rec odstrani_aux novi_av = function
    | [] -> Array.of_list (novi_element :: novi_av )
    | x ::xs -> if x.loc = av_element.loc then 
                  odstrani_aux novi_av xs
                else 
                  odstrani_aux (x::novi_av) xs
  in odstrani_aux [] (Array.to_list stari_av)
(*Zapise state.za_resevanje s pregledom unij -> naredi in uredi array z vsemi praznimi celicami zapisanimi v available obliki, sort glede na (dolzina av.possible)
  (-> resi tiste,kjer je najmanj moznosti, resi vse kjer je samo po ena moznost,ce je vec kot 1 pa izbira)*)
 (*ce izbira(izbira=state1), si neizbrane moznosti zapomne(state2) -> v state2 tudi ze zapise state.za_resevanje*)
 let branch_state (state : state) : (state * state option) option = 
     if state.za_resevanje =[||] then preglej_unije state ;
     print_state(state);
     print_newline ();
     let update_av = selection_sort_array state.za_resevanje in
     match odstrani_enake_in_uredi_enojne (update_av) with 
     | None -> None 
     | Some([],[]) -> None
     | Some([], drugi) -> let izbira = List.nth drugi 0 in 
                           (*ze prej bi prislo do napake ce bi bila prazni possiblesi*)
                          let element = List.nth (izbira.possible) 0 in
                          let x,y = izbira.loc in
                          let izpolnjena_cell = nova_grid (x) (y) (element) state.current_grid in
                          let boxi = boxes izpolnjena_cell in 
                          let box = (Array.of_list (boxi)).(izracun_boxa_iz_koordinat x y) in
                          if check_new(izpolnjena_cell.(x)) && check_new(izpolnjena_cell.(y)) && check_new(box) then 
                            let ostali = List.filter (fun x -> x <> element) izbira.possible in 
                            let novi_av = {loc=(x,y); possible=ostali} in 
                            let za_resevanje = odstrani (izbira) (update_av) (novi_av) in 
                            let novi_state_za_resevanje = [|[|za_resevanje|]|] in
                            let state1={problem=state.problem; current_grid=izpolnjena_cell; vrstice=cal_empty Vrstica izpolnjena_cell; stolpci= cal_empty Stolpec izpolnjena_cell; boxi=cal_empty Box izpolnjena_cell ;  za_resevanje=[||] } in
                            let state2 = {problem=state.problem; current_grid=copy_grid state.current_grid; vrstice=state.vrstice; stolpci=state.stolpci; boxi=state.boxi; za_resevanje=novi_state_za_resevanje} in
                            Some(state1, Some state2)
                          else
                            None
     | Some(enojci,_) -> let nova = izpolni_enojce (enojci) state.current_grid in
                         if check_all nova then 
                          let state_od_enojcev = {problem=state.problem; current_grid=nova; vrstice=cal_empty Vrstica nova; stolpci= cal_empty Stolpec nova; boxi=cal_empty Box nova ; za_resevanje=[||] } in
                          Some(state_od_enojcev,None)
                         else
                          None
                     
let depth = ref 0
 
 (* pogledamo, če trenutno stanje vodi do rešitve *)
 let rec solve_state (state : state) =
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
   | Some (st1, None) -> 
      match solve_state st1 with 
      | Some solution -> Some solution 
      | None -> None 

 let solve_problem (problem : problem) =
   problem |> initialize_state |> solve_state