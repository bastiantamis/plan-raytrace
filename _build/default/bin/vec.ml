type vec3 = {x:float; y:float; z:float}
let vec3 x y z = {x; y; z}

let from_angle theta phi =
  vec3 (cos phi *. cos theta) (cos phi *. sin theta) (sin phi)

let norm v =
  sqrt (v.x *. v.x +. v.y *. v.y +. v.z *. v.z)

let normalize v =
  let n = norm v in
  vec3 (v.x /. n) (v.y /. n) (v.z /. n)

let ( +^ ) u v =
  vec3 (u.x +. v.x) (u.y +. v.y) (u.z +. v.z)

let ( -^ ) u v =
  vec3 (u.x -. v.x) (u.y -. v.y) (u.z -. v.z)

let ( *^ ) u v = 
  u.x *. v.x +. u.y *. v.y +. u.z *. v.z

let ( *^. ) u a = 
  vec3 (u.x *. a) (u.y *. a) (u.z *. a)

let within p c s =
    Float.abs (c.x -. p.x) <= s && Float.abs (c.y -. p.y) <= s
