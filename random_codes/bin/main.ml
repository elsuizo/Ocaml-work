(* a two dimensional point *)
type point2d = { x: float; y: float }[@@deriving show]

(* calculate the norm of a 2d point *)
let magnitude { x = x_pos; y = y_pos } =
   Float.sqrt (x_pos ** 2. +. y_pos ** 2.)

let p = { x = 1.0; y = 3.0 };;

print_endline(show p)
