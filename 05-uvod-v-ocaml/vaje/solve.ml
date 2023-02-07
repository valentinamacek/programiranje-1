
(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)

   type available = { loc : int * int; mutable possible : int list }

   type objekt = Vrstica | Stolpec | Box 
   
   type lastnosti_objekta = { indeksi_praznih : (int * int) list array ; vrsta : objekt ; manjkajoci : int list array ; min_dolzina : int ; min_indeksi: int list}
   
 
 
   let manjkajoci (list: int list) = 
     let rec manjkajoci_aux acc = function
         | [] -> acc
         | [x] -> acc
         | x :: y :: xs -> if x + 1 = y then manjkajoci_aux (acc) (y :: xs)
           else manjkajoci_aux ((x+1) :: acc) ((x+1)::y::xs)
   in manjkajoci_aux [] (Model.selection_sort (0 ::(10) :: list))
   
   
 
   
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
         | Vrstica -> empty_rows_aux [] []  9 [] 0 (Model.rows grid) 
         | Stolpec -> empty_rows_aux [] [] 9 [] 0 (Model.columns grid)
         | Box -> empty_rows_aux [] [] 9 [] 0 (Model.boxes grid)
 
 let daj_na_izracun objekt = 
   let rec daj_na_izracun_aux acc = function
       | [] -> List.flatten acc
       | x :: xs-> daj_na_izracun_aux ((objekt.indeksi_praznih.(x))::acc) xs 
   in daj_na_izracun_aux [] objekt.min_indeksi
 
 let minimalna_dolzina_manjkajocih (grid) = 
   let vrstice = cal_empty Vrstica grid in
   let minimalen = ref vrstice.min_dolzina in
   let stolpci = cal_empty Stolpec grid in 
     if stolpci.min_dolzina < !minimalen then 
       minimalen:= stolpci.min_dolzina;
   let boxi = cal_empty Box grid in 
   if boxi.min_dolzina < !minimalen then 
     minimalen:= boxi.min_dolzina;
   !minimalen
 
 
 
 
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
 
 
 type state = { problem : Model.problem; current_grid : int option Model.grid ; vrstice : lastnosti_objekta ; stolpci : lastnosti_objekta ; boxi : lastnosti_objekta; mutable minimalni: (int list * objekt) list;  minimalen: int; mutable za_resevanje: available array array array} 
 
 let print_state (state : state) : unit =
   Model.print_grid
     (function None -> "?" | Some digit -> string_of_int digit)
     state.current_grid
 
 type response = Solved of Model.solution | Unsolved of state | Fail of state
 
 let initialize_state (problem : Model.problem) : state =
   { current_grid = Model.copy_grid problem.initial_grid; problem=problem ; vrstice=cal_empty Vrstica problem.initial_grid; stolpci= cal_empty Stolpec problem.initial_grid; boxi=cal_empty Box problem.initial_grid ; minimalni=[]; minimalen=minimalna_dolzina_manjkajocih (problem.initial_grid); za_resevanje=[||] }
 
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
 
 
 
 let nova_grid i j element grid = 
   Array.init 9 (fun vrstica -> Array.init 9 (fun st -> if st = j && i = vrstica then (Some element) else grid.(vrstica).(st)))    
 
 let odstrani av_element arra novi_element = 
   let rec odstrani_aux nova_arr = function
     | [] -> Array.of_list (novi_element :: nova_arr )
     | x ::xs -> if x.loc = av_element.loc then 
                   odstrani_aux nova_arr xs
                 else 
                   odstrani_aux (x::nova_arr) xs
 in odstrani_aux [] (Array.to_list arra)
             
 let izpolni_enojce lst_enojci grid = 
     let nova_grid = Model.copy_grid grid in
     let rec izpolni_enojce_aux = function
         | [] -> nova_grid
         | s:: xs -> let x,y = s.loc in 
                     let enojec = List.nth (s.possible) 0 in 
                       nova_grid.(x).(y) <- Some enojec;
                       izpolni_enojce_aux xs
   in izpolni_enojce_aux lst_enojci
 
 
   let zapisi_minimalne (state)=
   let mini = ref [] in
   if state.vrstice.min_dolzina = state.minimalen then 
     mini := (state.vrstice.min_indeksi, Vrstica)::!mini;
   if state.stolpci.min_dolzina = state.minimalen then 
     mini := (state.stolpci.min_indeksi, Stolpec)::!mini;
   if state.boxi.min_dolzina = state.minimalen then
     mini := (state.boxi.min_indeksi, Box)::!mini; 
   state.minimalni <- !mini
   
   let prazne_moznosti sez_noneov (state) = 
     let rec prazne_moznosti_aux acc = function
         | [] ->  Array.of_list acc
         | (x,y) :: xs -> let skupni = presek3eh (state.vrstice.manjkajoci.(x)) (state.stolpci.manjkajoci.(y)) (state.boxi.manjkajoci.((Model.izracun_boxa_iz_koordinat x y))) in 
                           let podatekxy = {loc=(x,y); possible= skupni}
                           in 
                           prazne_moznosti_aux (podatekxy::acc) (xs)
   in prazne_moznosti_aux [] sez_noneov
   
   
   let za_vsak_min_objekt_izracunaj indeksi (objekt) (state)= 
     let rec daj_na_izracun_aux acc = function
         | [] -> Array.of_list acc
         | x::xs ->let rez = prazne_moznosti (objekt.indeksi_praznih.(x)) (state) in
                   daj_na_izracun_aux (rez::acc) (xs)
   in daj_na_izracun_aux [] indeksi 
   
   let za_vse_minimalne (state) = 
     let rec za_vse_minimalne_aux acc = function
         | [] -> state.za_resevanje <- (Array.of_list acc)
         | (x,y)::xs -> match y with 
                         | Vrstica -> let vsi_od_vrstic = za_vsak_min_objekt_izracunaj (x) (state.vrstice) (state) in
                                       za_vse_minimalne_aux (vsi_od_vrstic::acc) xs
                         | Stolpec -> let vsi_od_stolpcev = za_vsak_min_objekt_izracunaj (x) (state.stolpci) (state) in
                         za_vse_minimalne_aux (vsi_od_stolpcev::acc) xs
                         | Box -> let vsi_od_boxov = za_vsak_min_objekt_izracunaj (x) (state.boxi)  (state) in
                         za_vse_minimalne_aux (vsi_od_boxov::acc) xs
   in za_vse_minimalne_aux [] state.minimalni
   
 
   (*Naslednje funkcije izracunajo unije seznamov  *)
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
   
   let preglej_unije_podmnozic_minov (state) (*(arr : available array array array)*) = 
     zapisi_minimalne state;
     za_vse_minimalne state;
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
     
 
 (*prvo stanje, drugo stanje*)
   (* TODO: Pripravite funkcijo, ki v trenutnem stanju poišče hipotezo, glede katere
      se je treba odločiti. Če ta obstaja, stanje razveji na dve stanji:
      v prvem predpostavi, da hipoteza velja, v drugem pa ravno obratno.
      Če bo vaš algoritem najprej poizkusil prvo možnost, vam morda pri drugi
      za začetek ni treba zapravljati preveč časa, saj ne bo nujno prišla v poštev. *)
 
 (*prazno celico v gridu nastavi na možno rešitev*)
 
 
 let branch_state (state : state) : (state * state option) option = 
     if state.za_resevanje =[||] then preglej_unije_podmnozic_minov state ;
     let update_av = selection_sort_array state.za_resevanje in
     match odstrani_enake_in_uredi_enojne (update_av) with 
     | None -> None 
     | Some([],[]) -> None
     | Some([], drugi) -> let izbira = List.nth drugi 0 in 
                           (*ze prej bi prislo do napake ce bi bila prazni possiblesi*)
                          let element = List.nth (izbira.possible) 0 in
                          let x,y = izbira.loc in
                          let izpolnjena_cell = nova_grid (x) (y) (element) state.current_grid in
                          let ostali = List.filter (fun x -> x <> element) izbira.possible in 
                          let novi_av = {loc=(x,y); possible=ostali} in 
                          let za_resevanje = odstrani (izbira) (update_av) (novi_av) in 
                          let novi_state_za_resevanje = [|[|za_resevanje|]|] in
                          let state1={problem=state.problem; current_grid=izpolnjena_cell; vrstice=cal_empty Vrstica izpolnjena_cell; stolpci= cal_empty Stolpec izpolnjena_cell; boxi=cal_empty Box izpolnjena_cell ; minimalni=[]; minimalen=minimalna_dolzina_manjkajocih izpolnjena_cell; za_resevanje=[||] } in
                          let state2 = {problem=state.problem; current_grid=Model.copy_grid state.current_grid; vrstice=state.vrstice; stolpci=state.stolpci; boxi=state.boxi; minimalni=state.minimalni; minimalen=state.minimalen; za_resevanje=novi_state_za_resevanje} in
                          Some(state1, Some state2)
     | Some(enojci,_) -> let nova = izpolni_enojce (enojci) state.current_grid in
                         let state_od_enojcev = {problem=state.problem; current_grid=nova; vrstice=cal_empty Vrstica nova; stolpci= cal_empty Stolpec nova; boxi=cal_empty Box nova ; minimalni=[]; minimalen=minimalna_dolzina_manjkajocih nova; za_resevanje=[||] } in
                         Some(state_od_enojcev,None)
                         
                     
 
 
 (* pogledamo, če trenutno stanje vodi do rešitve *)
 let rec solve_state (state : state) =
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
   | Some (st1, None) -> solve_state st1 
 
 let solve_problem (problem : Model.problem) =
   problem |> initialize_state |> solve_state
 