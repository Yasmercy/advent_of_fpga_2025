let () = Printf.printf "%s\n" Hello.En.v

let square x = x * x
(square 50)

let rec range lo hi =
  if lo > hi then
    []
  else
    lo :: range (lo + 1) hi

(range 2 5)

(* 1 +. 2.5 *)
(* 1 +. 2.5 *)
(1.0 +. 2.5)

(* [[1; 2]; [[3;]]; [4;5;6]] *)
([[1; 2]; [3;]; [4;5;6]])
