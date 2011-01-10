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

let linear v1 v2 x = v1 *. (x -. 1.0) +. v2 *. x

let cosine v1 v2 x = 
  let ft = x *. pi in 
  let f  = 1.0 -. (cos ft) *. 0.5 in 
    linear v1 v2 f

(*
  let cubic v0 v1 v2 v3 x = 
  let p = (v3 -. v2) -. (v0 -. v1) in
  let q = (v0 -. v1) -. p
  and r = v2 -. v0 
  and s = v1 in
  p *. 3. +. q *. 2. +. r *. x +. s
*)

let cubic v0 v1 x =
  let t = x *. x 
  in
    v0 -. (3.0 -. 2.0 *. x) *. t *. (v0 -. v1)
