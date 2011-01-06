open Sdl;;
open Sdlvideo;;
open Sdlevent;;
open Sdlloader;;
open Sdlkey;;
open Noise;;

module I : (INTERPOLATE with type t = float) = 
struct 
  type t = float
  let  f = Interpolate.cubic
end 

module N = Noise2(I)

type point = (int * int)

(* texture generation module 
   we use perlin noise to get the value of each pixel:
   a color is an int triple (actually a quadruple), so we need a way to 
   produce 3 different values out of the Noise.f function
   => a seed parameter is necessary to change the Noise.f behaviour
   for now, we'll just generate a b&w texture
   the result is a 32bit SDL surface
*)
module Texture_gen =
struct
  type t = Sdlvideo.surface
      (*
	let create w h = create_RGB_surface [ `ASYNCBLIT ; `HWSURFACE ] w h 32 0x000000FF 0x0000FF00 0x00FF0000 0xFF000000
      *)
  let put s (x,y) c = 
    put_pixel s ~x ~y (map_RGB s c)

  let get_dims s =
    let s_info = surface_info s in
      s_info.w, s_info.h

(*
  let fill s = 
    let k = 50 in
    let width, height = get_dims s
    in
      Sdlvideo.lock s;
      for y = 1 + k to height + k do
	for x = 1 + k to width + k do 
	  let x' = (float)x /. (float)k
	  and y' = (float)y /. (float)k
	  in
	  let r,g,b = 
	    let v = int_of_float ((N.D2.perlin_noise (x', y')) *. 255.0)
	    in 
	      v, v, v
	  in
	    put s (x-1-k,y-1-k) (r,g,b)
	done
      done;
      Sdlvideo.unlock s
*)

  let fill s = 
    let width, height = get_dims s
    in
      Sdlvideo.lock s;
      for y = 1 to height do
	for x = 1 to width do 
	  let x' = float x
	  and y' = float y
	  and z' = float (width - x)
	  and w' = float (height - y)
	  in
	    (* color component *)
	  let color_comp x y = int_of_float (((N.D2.perlin (x,y)) *. 127.0) +. 127.0)
	  in
	  let r = color_comp x' y'
	  and g = color_comp y' x'
	  and b = color_comp z' w'
	  in
	    put s (x-1,y-1) (r,g,b)
	done
      done;
      Sdlvideo.unlock s

end

let debug x = prerr_string (x);flush stderr ;;

Sdl.init [ `EVERYTHING ]

let screen_width  = 300
let screen_height = 200 
let screen_depth  = 32 

let vidsurf  =  set_video_mode screen_width screen_height [ `ANYFORMAT ]
  
let display () = 
  flip vidsurf 

let _ = 
  Texture_gen.fill vidsurf;
  while true do
    display ();
    let ev_process e =
      match e with
	  VIDEOEXPOSE -> (debug "displaying\n";display ())
	| QUIT        -> exit 0
	| KEYDOWN k   -> (
	    match k.keysym with 
		KEY_ESCAPE
	      | KEY_q  -> exit 0
	      | KEY_r  -> display ()
	      | _      -> ()
	  )
	| MOUSEBUTTONDOWN m  -> (
	  )
	| MOUSEBUTTONUP   m  -> (
	  )
	| _                  -> ()
    and ev_list = Sdlevent.wait_event () in
      ev_process ev_list 
  done
(*
  for i = 0 to screen_width do
    for j = 0 to screen_height do
      print_string (string_of_float (N.D2.gen 1 (i,j)));
      print_newline ()
    done
  done
  *)
