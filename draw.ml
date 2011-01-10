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
open Sdl
open Sdlvideo

(* sdl video drawing toolbox *)
module Draw =
struct

  let point s x y c = put_pixel s ~x ~y c 

  (* bresenham line drawing? *)
  let line s x1 y1 x2 y2 c =
    let dx   = abs (x2 - x1)
    and dy   = abs (y2 - y1)
    and incx = if x2 < x1 then pred else succ
    and incy = if y2 < y1 then pred else succ
    in
    let rec draw_h s x y len c =
      if len = 0 
      then ()
      else (
	point s x y c;
	draw_h s x (incy y) (pred len) c
      )
    in
    let rec draw_v s x y len c =
      if len = 0 
      then ()
      else (
	point s x y c;
	draw_v s (incx x) y (pred len) c
      )
    in
    let rec draw_xy s x y len c =
      if len = 0 
      then ()
      else (
	point s x y c;
	draw_xy s (incx x) (incy y) (pred len) c
      )
    in
    let rec draw_x s dx dy x y acc xm ym c =
	if (x = xm) && (y = ym) then ()
	else 
	  if (acc < dx) 
	  then (
	    point s (incx x) y c;
	    draw_x s dx dy (incx x) y (acc + dy) xm ym c
	  )
	  else (
	    point s x (incy y) c;
	    draw_x s dx dy x (incy y) (acc - dx) xm ym c
	  )
    in
    let rec draw_y s dx dy x y acc xm ym c =
	if (x = xm) && (y = ym) then ()
	else 
	  if (acc < dy) 
	  then (
	    point s x (incy y) c;
	    draw_y s dx dy x (incy y) (acc + dx) xm ym c
	  )
	  else (
	    point s (incx x) y c;
	    draw_y s dx dy (incx x) y (acc - dy) xm ym c
	  )
    in
      Sdlvideo.lock s;
      begin
	match dx, dy with
	    0, 0            -> ()
	  | 0, _            -> draw_h s x1 y1 dy c
	  | _, 0            -> draw_v s x1 y1 dx c
	  | a, b when a = b -> draw_xy s x1 y1 dx c
	  | a, b when a < b -> draw_y s dx dy x1 y1 (dy lsr 1) x2 y2 c
	  | _,_             -> draw_x s dx dy x1 y1 (dx lsr 1) x2 y2 c
      end;
      Sdlvideo.unlock s

  let rect s x1 y1 x2 y2 c =
    line s x1 y1 x1 y2 c;
    line s x1 y1 x2 y1 c;
    line s x2 y1 x2 y2 c; 
    line s x1 y2 x2 y2 c
    
  let fill_rect s x1 y1 x2 y2 c =
    for i = x1 to x2 do
      line s i y1 i y2 c
    done


end	
