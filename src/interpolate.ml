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
let pi = acos (-. 1.)

(* ax + b(1-x) 
   ax + b - bx
   (a - b)x + b
*)
let linear b a x = (a -. b) *. x +. b

let cosine v0 v1 x = 
  let ft = x *. pi *. 0.5 in  (* range : [0;pi/2] *)
  let f  = 1.0 -. (cos ft) in (* range : [0;1] *)
    linear v0 v1 f

(*
  let cubic v0 v1 v2 v3 x = 
  let p = (v3 -. v2) -. (v0 -. v1) in
  let q = (v0 -. v1) -. p
  and r = v2 -. v0 
  and s = v1 in
  p *. 3. +. q *. 2. +. r *. x +. s
*)

(* 3.t^2 - 2.t^3 *)
let cubic v0 v1 x =
  let x2 = x *. x
  in
    (v1 -. v0) *. x2 *. (3. -. 2. *. x) +. v0
(*
  in
  let fx = 3. *. x2 -. 2. *. x2 *. x
  in
    (v1 -. v0) *. fx +. v0
*)
(*    linear v0 v1 fx  
      = v1 *. fx -. v0 *. fx +. v0
      = v1 *. 3. *. x2 -. v1 *. 2. *. x2 *. x -.
        v0 *. 3. *. x2 +. v0 *. 2. *. x2 *. x +.      
        v0
      = (v1 -. v0) *. 3. * x2 - (v1 -. v0) *. 2. *. x2 *. x +. v1
      = (v1 -. v0) *. x2 *. (3. -. 2. *. x) +. v1
*)


(* f(t) = 6.t^5 - 15.t^4 + 10.t^3 *)
let quintic v0 v1 x =
  let t2 = x *. x
  in
  let t4 = t2 *. t2
  and t3 = t2 *. x
  in
  let t5 = t4 *. x
  in
    linear v0 v1 (6. *. t5 -. 15. *. t4 +. 10. *. t3)



