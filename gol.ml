
open Tsdl

let alive_color = 0xFF0000FFl
let dead_color = 0x000000FFl

(* modulus that handles both positive and negative numbers *)
let modulo a b =
    let res = a mod b in
    if res >= 0 then res else res + b

(* Checks if cell/pixel is alive or dead *)
let is_alive cell =
    if cell = alive_color then 1 else 0

(* Execute this to update the "next_gen" array passed in 
   for the next game of life generation.*)
let check_cell next_gen curr_gen i width ending =
    let topleft = modulo (i - 1 - width) ending in
    let top = modulo (i - width) ending in
    let topright = modulo (i + 1 - width) ending in
    let left = modulo (i - 1) ending in
    let center = modulo i ending in
    let right = modulo (i + 1) ending in
    let bottomleft = modulo (i - 1 + width) ending in
    let bottom = modulo (i + width) ending in
    let bottomright = modulo (i + 1 + width) ending in

    let neighbor_sum =
        ( is_alive (Bigarray.Array1.get curr_gen topleft) ) +
        ( is_alive (Bigarray.Array1.get curr_gen top) ) +
        ( is_alive (Bigarray.Array1.get curr_gen topright) ) +
        ( is_alive (Bigarray.Array1.get curr_gen left) ) +
        ( is_alive (Bigarray.Array1.get curr_gen right) ) +
        ( is_alive (Bigarray.Array1.get curr_gen bottomleft) ) +
        ( is_alive (Bigarray.Array1.get curr_gen bottom) ) +
        ( is_alive (Bigarray.Array1.get curr_gen bottomright) ) in

    if neighbor_sum == 3 then
        Bigarray.Array1.set next_gen center alive_color
    else if neighbor_sum == 2 && (is_alive (Bigarray.Array1.get curr_gen center)) == 1 then
        Bigarray.Array1.set next_gen center alive_color
    else
        Bigarray.Array1.set next_gen center dead_color

(* Recursivley go through each cell/pixel and run "check_cell" on it *)
let rec gol_generation_shift next_gen curr_gen i width ending =
    if i >= ending then
        ()
    else
        let _ = check_cell next_gen curr_gen i width ending in
        gol_generation_shift next_gen curr_gen (i+1) width ending

let render curr_gen next_gen w h pitch depth r =
    Sdl.delay 100l;

    (* Convert array with RGB values to a texture and render to window (texture up-scales to fit whole window automatically) *)
    match Sdl.create_rgb_surface_from curr_gen ~w:w ~h:h ~pitch:pitch ~depth:depth 0xFF000000l 0x00FF0000l 0x0000FF00l 0x000000FFl with
    | Error _ -> Sdl.log "Error create_rgb_surface_from"
    | Ok s ->
        match Sdl.create_texture_from_surface r s with
        | Error (`Msg e) -> Sdl.log " Could not create texture: %s" e
        | Ok t ->
            assert (Sdl.render_copy r t = Ok ());
            Sdl.render_present r;

    (* Make sure "next_gen" is empty (all dead cells) before performing generation shift*)
    let _ = Bigarray.Array1.fill next_gen dead_color in

    (* Figure out what the next generation looks like based on "curr_gen", and store it in "next_gen" *)
    gol_generation_shift next_gen curr_gen 0 w (w*h);

    (* Replace "curr_gen" with "next_gen" *)
    Bigarray.Array1.blit next_gen curr_gen

let main () = match Sdl.init Sdl.Init.video with
    | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
    | Ok () ->
        match Sdl.create_window ~w:640 ~h:480 "SDL OpenGL" Sdl.Window.opengl with
        | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
        | Ok win ->
            match Sdl.create_renderer win with
            | Error (`Msg e) -> Sdl.log " Could not create renderer: %s" e
            | Ok r ->
                let w = 32 in
                let h = 24 in

                (* curr_gen = Current generation. Stores what the current generation of game of life looks like,
                              in terms of, which cells/pixels are alive vs dead *)
                let curr_gen = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout (w*h) in
                (* next_gen = Next generation. Used to temporarily hold the next generation when a generation-shift is happening. *)
                let next_gen = Bigarray.Array1.create (Bigarray.Array1.kind curr_gen) (Bigarray.Array1.layout curr_gen) (Bigarray.Array1.dim curr_gen) in
                let _ = Bigarray.Array1.fill curr_gen dead_color in

                (* 3 cell stick that rotate forever, called "blinker" *)
                (*let _ = Bigarray.Array1.set curr_gen (14+w+w) alive_color in
                let _ = Bigarray.Array1.set curr_gen (15+w+w) alive_color in
                let _ = Bigarray.Array1.set curr_gen (16+w+w) alive_color in*)
                
                (* Called Glider/spaceship (Just keeps walking in a direction) *)
                let _ = Bigarray.Array1.set curr_gen (14+w+w) alive_color in
                let _ = Bigarray.Array1.set curr_gen (15+w+w) alive_color in
                let _ = Bigarray.Array1.set curr_gen (16+w+w) alive_color in
                let _ = Bigarray.Array1.set curr_gen (16+w) alive_color in
                let _ = Bigarray.Array1.set curr_gen 15 alive_color in

                (* pitch = NORMALLY this is number of bytes for each row, it says quite clearly 
                           in the code example on http://wiki.libsdl.org/SDL_CreateRGBSurfaceFrom.
                           
                           BUT if we look inside "tsdl.mli", which is essentially the api of the sdl we 
                           interact with here in ocaml, there is a note that says:
                           "{b Note} The pitch is given in bigarray elements {b not} in bytes." 
                           
                           SO pitch should equal the size of the row (number of cells, elements) not
                           the number of bytes, in order to get correct behaviour here. *)
                let pitch = w in

                (* depth = number of bits in each pixel *)
                let depth = (8 * (Bigarray.kind_size_in_bytes (Bigarray.Array1.kind curr_gen) ) ) in

                let ev = Sdl.Event.create () in
                let rec poll_loop event =
                    let new_event = Sdl.poll_event (Some event) in
                    if new_event && Sdl.Event.enum (Sdl.Event.get event Sdl.Event.typ) = `Quit then
                        ()
                    else
                        let _ = render curr_gen next_gen w h pitch depth r in
                        poll_loop event
                in
                poll_loop ev

let () = main ()
