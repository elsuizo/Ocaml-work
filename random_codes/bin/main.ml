(* a two dimensional point *)
type point2d = { x: float; y: float }[@@deriving show]

(* calculate the norm of a 2d point *)
let magnitude { x = x_pos; y = y_pos } =
   Float.sqrt (x_pos ** 2. +. y_pos ** 2.)

let distance v1 v2 =
   magnitude { x = v1.x -. v2.x; y = v1.y -. v2.y }

let p = { x = 1.0; y = 3.0 }

type circle_desc = { center: point2d; radius: float }
type rect_desc   = { lower_left: point2d; width: float; height: float }
type segment_desc = { endpoint1: point2d; endpoint2: point2d }

type scene_element =
| Circle of circle_desc
| Rect of rect_desc
| Segment of segment_desc

let is_inside_scene_element point scene_element =
   let open Float.O in
   match scene_element with
      | Circle { center; radius } -> distance center point < radius
      | Rect { lower_left; width; height } -> point.x > lower_left.x && point.x < lower_left.x + width && lower_left.y && point.y < lower_left.y + height
      | Segment _ -> false
