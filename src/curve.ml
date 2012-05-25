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

external identity : float -> float = "%identity"

let cosine x = cos x

(* 3.t^2 - 2.t^3 *)
let cubic x =
  let x2 = x *. x in
  x2 *. (3. -. 2. *. x)

(* f(t) = 6.t^5 - 15.t^4 + 10.t^3 *)
let quintic x =
  let t2 = x *. x in
  let t4 = t2 *. t2
  and t3 = t2 *. x in
  let t5 = t4 *. x in
  6. *. t5 -. 15. *. t4 +. 10. *. t3


