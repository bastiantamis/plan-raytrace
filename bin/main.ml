open Const

open Vec
open Graphics
open Elements
open Raycasting

open Interface

let () = 
    Random.self_init();

    open_graph " "; resize_window size (size + height); auto_synchronize false;
    clear_graph ();

    let elements = Array.make 10 Rien in
    for k = 0 to Array.length elements - 1 do
        elements.(k) <- rand_sphere sizef;
    done;
    let arbre = cons 4 (vec3 (sizef /. 2.) (sizef /. 2.) (sizef /. 2.)) (sizef /. 2.) elements in

    print_arbre arbre;

    draw_arbre arbre;
    draw_division arbre;
    
    let n = ref 0. in
    while not (key_pressed () && read_key () = ' ') do
        let t = Sys.time () in

        (*if key_pressed () then
            (theta := !theta +. 0.1 *. (if read_key () = 'q' then 1. else 0. +. if read_key () = 'd' then -.1. else 0.);
            (if read_key () = 'z' then o := !o +^ vec3 (cos !theta) (sin !theta) 0. *^. 5.));*)

        let x, y = mouse_pos () in
        o := vec3 (float_of_int x) (float_of_int y) 0.;

        p := 0;
        
        draw_screen arbre;

        write_int (int_of_float (1. /. (Sys.time () -. t))) 0;
        write_int !p 1;

        synchronize ();
        n := !n +. 1.;
    done;

    print_newline ();
    print_string "Ips moyen: ";
    print_float (!n /. Sys.time ());
    print_newline ()