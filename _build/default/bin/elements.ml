open Vec
open Graphics

type element = Rien | Sphere of vec3 * vec3 * float * color
type arbre = Vide | Feuille of element array | Noeud of vec3 * float * arbre list

let rand_sphere sizef = 
    let r = 20. in
    let borne = sizef -. 2. *. r in
    let c = vec3 (r +. Random.float borne) (r +. Random.float borne) 0. in
    let v = from_angle (Random.float 3.15) (0.) *^. (Random.float 2.) in
    let col = rgb (Random.int 200) (Random.int 200) (Random.int 200) in
    Sphere (c, v, r, col)
    
let check o s t =
    let rec aux k =
        if k = Array.length t then [] else 
            match t.(k) with
            | Rien -> aux (k + 1) 
            | Sphere (c, _, r, _) -> if within c o s then t.(k) :: aux (k + 1) else aux (k + 1)
    in Array.of_list (aux 0 )


let rec cons k o s t =
    if k = 1 then 
        Feuille (check o s t) 
    else 
        let s', k' = s /. 2., k - 1 in
          let a, b, c, d = 
            cons k' (o +^ vec3 s' s' 0.) s' t,
            cons k' (o +^ vec3 s' (-.s') 0.) s' t,
            cons k' (o +^ vec3 (-.s') s' 0.) s' t,
            cons k' (o +^ vec3 (-.s') (-.s') 0.) s' t in
          if a = b && b = c && c = d && d = Feuille [||] then Vide else Noeud (o, s, [a; b; c; d])

let step e bound =
    match e with
    | Rien -> Rien
    | Sphere (c, v, r, col) ->
        (let c' = c +^ v in
        let v' = vec3 ((if c'.x < r || c'.x > bound -. r then -1. else 1.) *. v.x) 
            ((if c'.y < r || c'.y > bound -. r then -1. else 1.) *. v.y) 
            0. in
        Sphere (c', v', r, col))
