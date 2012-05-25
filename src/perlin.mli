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

module type CONFIG =
sig
  val random_int : int -> int
  val random_float : float -> float
  val curve : float -> float
  val psize : int
end

module type S =
sig 
  type v 
  val perlin : v -> float 
  val fbm : int -> float -> v -> float 
end

module Make :
  functor (Config : CONFIG) ->
sig

  module D1 :
  sig
    type v = float
    val perlin : v -> float
    val fbm : int -> float -> v -> float
  end

  module D2 :
  sig
    type v = float * float
    val perlin : v -> float
    val fbm : int -> float -> v -> float
  end

  module D3 :
  sig
    type v = float * float * float
    val perlin : v -> float
    val fbm : int -> float -> v -> float
  end

(*
  module type VEC =
  sig
    val size : int
    type 'a t
    val get : 'a t -> int -> 'a
    val set : 'a t -> int -> 'a -> unit
    val init : (int -> 'a) -> 'a t
    val dot : 'a t -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val map2i : (int -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
    val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  end

  module Dn :
    functor (V : VEC) ->
  sig
    type v = float V.t
    val perlin : v -> float
    val fbm : int -> float -> v -> float
  end
*)
end
