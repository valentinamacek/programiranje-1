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

  (*Pogledamo prvo prazno celico v prvi nepolni vrstici(vrstica ki vsebuje prazne celice)
in poiscemo njene manjkajoce elemente, ki jih zapisemo v state.available*)

let branch_state state =
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


(*vrne int list z indeksi vrstic, ki imajo vsaj eno celico neizpolnjeno*)
let cal_empty vrsta_objekta grid = 
  let rec empty_aux acc index (*dobis list of arrays*) = function
      | [] -> List.rev acc
      | x :: xs -> if (Array.exists (Option.is_none) x ) then (empty_aux (index :: acc) (index + 1) xs )
                    else (empty_aux (acc) (index + 1) xs)
in empty_aux [] 0 (vrsta_objekta grid) 