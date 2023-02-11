let is_valid_move grid x y el =
  let row_ok = Array.for_all (fun elt -> elt <> Some el) grid.(x) in
  let col_ok = Array.for_all (fun row -> row.(y) <> Some el) grid in
  let x0 = (x / 3) * 3 in
  let y0 = (y / 3) * 3 in
  let square_ok =
    Array.for_all
      (fun i ->
         Array.for_all
           (fun j -> grid.(x0 + i).(y0 + j) <> Some el)
           [| 0; 1; 2 |])
      [| 0; 1; 2 |]
  in
  row_ok && col_ok && square_ok