let square x = x * x
let () = square 50 |> Printf.printf "%d\n"
let rec sum u = match u with [] -> 0 | x :: v -> x + sum v
let () = sum [ 1; 3; 5 ] |> Printf.printf "%d\n"
