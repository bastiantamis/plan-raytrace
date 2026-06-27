open Graphics
open Elements

let draw_arr t =
    for k = 0 to Array.length t - 1 do
        (*t.(k) <- step t.(k);*)
        match t.(k) with
        | Rien -> ()
        | Sphere (c, v, r, col) -> (set_color col; fill_circle (int_of_float c.x) (int_of_float c.y) (int_of_float r));
    done

let rec draw_arbre a =
    match a with 
    | Vide -> ()
    | Feuille t -> draw_arr t
    | Noeud (c, s, l) -> List.iter draw_arbre l

let rec draw_division a =
    match a with 
    | Vide | Feuille _ -> ()
    | Noeud (c, sf, l) -> (
        let x, y, s = int_of_float c.x, int_of_float c.y, int_of_float sf in
        let u = 200 - s in 
        set_color (rgb u u u);
        draw_circle x y s;
        List.iter draw_division l)

let rec print_arbre a =
    match a with
    | Vide -> ()
    | Feuille t -> if Array.length t = 0 then () else print_int (Array.length t)
    | Noeud (c, s, l) -> (print_char '('; List.iter print_arbre l; print_string "), ")

let write_int x i =
    set_color white; fill_rect 0 (20 * i) 64 (20 * (i + 1)); moveto 0 (20 * i); set_color black; draw_string (string_of_int x)
