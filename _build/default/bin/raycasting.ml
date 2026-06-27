open Graphics
open Vec
open Elements
open Const

let ray_from theta phi i j =
    from_angle (!theta -. (float_of_int i /. widthf -. 0.5) *. fov_theta) (!phi -. (0.5 -. float_of_int j /. heightf) *. fov_phi)

let solve_sphere d c r =
  let c' = !o -^ c in
  let b, l = d *^ c', norm c' in
  let t = -. (b +. sqrt (b *. b -. l *. l +. r *. r)) in
  t

let ray d t =
    let x, col = ref 1000., ref white in
    for k = 0 to Array.length t - 1 do
        match t.(k) with
        | Rien -> ()
        | Sphere (c, _, r, col') -> let x' = solve_sphere d c r in
            if x' > 0. && x' < !x then (x := x'; col := col')
    done;
    !x, !col

let hits_square o d c s =
    (*Slab method*)
    let x = (c.x +. s -. o.x) /. d.x in
    let x' = (c.x -. s -. o.x) /. d.x in
    let y = (c.y -. s -. o.y) /. d.y in
    let y' = (c.y +. s -. o.y) /. d.y in
    let low = Float.max (Float.min x x') (Float.min y y') in
    let high = Float.min (Float.max x x') (Float.max y y') in
    high >= 0. && low <= high

let p, p0 = ref 0, ref 1

let rec march_arbre d a =
    p := !p + 1;
    match a with
    | Vide -> -.1., magenta
    | Feuille t -> ray d t
    | Noeud (c, s, l) ->
        if hits_square !o d c s then march_list d l else -.1., magenta

and march_list d l =
    match l with
    | [] -> -1., white
    | h :: t -> let xa, cola = march_arbre d h in 
        let xl, coll = march_list d t in
        if xa > 0. && (xl < 0. || xa < xl) then xa, cola else xl, coll

let raycast i j a =
    let d = ray_from theta phi i j in
    let _, col = march_arbre d a in
    col

let draw_screen arbre =
  for j = 0 to height do
            for i = 0 to width do
                set_color (raycast i j arbre);
                plot i (size + j)
            done;
        done;