(*
  a perlin noise library

  Copyright (C) 2010  Didier Cassirame

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.

*)
open Sdl;;
open Sdlvideo;;
open Sdlevent;;
open Sdlloader;;
open Sdlkey;;
open Noise;;

module NConfig : (Noise.CONFIG) = 
struct 
  let interpolate = Interpolate.cubic
  let persistence = 1.0
  let octaves     = 6
  let psize       = 256
end 

module N = Noise.Make(NConfig)

type point = (int * int)

(* texture generation module 
   we use perlin noise to get the value of each pixel:
   a color is an int triple (actually a quadruple), so we need a way to 
   produce 3 different values out of the Noise.f function
   => a seed parameter is necessary to change the Noise.f behaviour
   the result is a 32bit SDL surface
*)
module Texture_gen =
struct
  type t = Sdlvideo.surface
      (*
	let create w h = create_RGB_surface [ `ASYNCBLIT ; `HWSURFACE ] 
	w h 32 0x000000FF 0x0000FF00 0x00FF0000 0xFF000000
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
	  and g = color_comp y' z'
	  and b = color_comp x' w'
	  in
	    put s (x-1,y-1) (r,g,b)
	done
      done;
      Sdlvideo.unlock s

end

let debug x = prerr_string (x);flush stderr ;;

Sdl.init [ `EVERYTHING ]

let screen_width  = 800
let screen_height = 600 
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
