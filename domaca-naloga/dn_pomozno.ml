type 'a grid = 'a Array.t Array.t
  ;;
type 'a grid = 'a Array.t Array.t
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
  Array.init 3 (fun vrstica -> l.(vrstica).(index mod 3))

  let boxes grid = List.init 9 (get_box grid)



  
