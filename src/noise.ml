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

let random_int = Random.int
let random_float = Random.float

(* integer power function *)
(*
let ipow x y = 
  let rec ipow_ x y =
  if y = 0 
  then 1 
  else 
    let k = ipow_ x (y / 2) 
    in
      if y mod 2 != 0 
      then k * k * x
      else k * k
  in
    if y < 0 
    then invalid_arg "this integer power function cannot accept negative power values"
    else ipow_ x y
*)

module type CONFIG =
sig

  val interpolate : float -> float -> float -> float
  val psize       : int

end

module type S =
sig

  type v 

  val perlin : v -> float

  val fbm    : int -> float -> v -> float

end

module Make (Config : CONFIG) = 
struct

  let b             = Config.psize
  let size          = b + b + 2
  let interpolate   = Config.interpolate

  (* permutation table --- *)
  let p = 
    let init_p p =
      for i = 0 to pred size do
	let j = random_int size in 
	let s = p.(i) in
	  p.(i) <- p.(j);
	  p.(j) <- s
      done;
      p
    in
    init_p (Array.init size (fun i -> i))


  let clamp v vmin vmax = 
    max (min v vmax) vmin


  (* generic gradient calculation module *)
  module Gradient =
  struct

    type t = float array
	
    (* vector scaling *)
    let scale v f =
      Array.iteri (fun i x -> v.(i) <- x *. f) v

    (* generic n dimensions dot product of v with itself *)
    let dot v = 
      Array.fold_left (fun acc k -> acc +. k *. k) 0. v

    let random_x () = 
      let r = random_int (b * 7) in
      float ((r mod (b * 2)) - b) /. (float b)

    (* compute 1 gradient *)
    let random dim =
      let tmp = Array.init dim (fun _ -> random_x ()) in
      let m = dot tmp in
      scale tmp (1. /. (sqrt m));
      tmp

  end

  (* one dimensional perlin noise - FIX-ME: I've never been used, check-me! *)
  module D1 : S with type v = float =
  struct

    type v = float

    (* gradients *)
    let gx = 
      Array.init size (fun _ -> Gradient.random_x ())

    (* perlin noise *)
    let perlin x =
      let x0 = floor x
      in
	(* Compute the integer positions of the 2 surrounding points *)
      let qx0 = int_of_float x0
      in
      let qx1 = succ qx0
      in
	(* Permutate values to get indices to use with the gradient look-up tables *)
      let q0 = p.(qx0 mod size)
      and q1 = p.(qx1 mod size)
      in
	(* Computing vectors from the 2 points to the input point *)
      let tx0 = x -. x0
      in
      let tx1 = tx0 -. 1.
      in
	(* Compute the dot-product between the vectors and the gradients *)
      let v0 = gx.(q0) *. tx0
      and v1 = gx.(q1) *. tx1
      in
	(* Do the interpolation to get the final value *)
	interpolate v0 v1 tx0

    (* fractional brownian motion *)
    let fbm octaves persistence x =
      let rec calc_sum i s =
	if i > octaves
	then s
	else
          let frequency =  2.0 ** (float i)
          and amplitude =  persistence ** (float i)
	  in
	  let s = s +. (perlin (x *. frequency)) *. amplitude
	  in
            calc_sum (succ i) s
      in
      let r = (calc_sum 1 0.)
      in
	clamp r (-.1.) 1.

  end

  (* ----------------------------------------------------------------------------- *)

  (* two dimensional perlin noise *)
  module D2 : S with type v = float * float =
  struct

    type v = float * float

    (* gradients *)
    let gx, gy = 
      let gd = Array.init size (fun _ -> Gradient.random 2) in
      Array.init size (fun i -> gd.(i).(0)),
      Array.init size (fun i -> gd.(i).(1))

    (* perlin noise *)
    let perlin (x, y) =
      let x0 = floor x
      and y0 = floor y in
      (* Compute the integer positions of the four surrounding points *)
      let qx0 = int_of_float x0
      and qy0 = int_of_float y0 in
      let qx1 = succ qx0
      and qy1 = succ qy0 in
      (* Permutate values to get indices to use with the gradient look-up tables *)
      let q00 = p.((qy0 + p.(qx0 mod size)) mod size)
      and q01 = p.((qy0 + p.(qx1 mod size)) mod size)
      and q10 = p.((qy1 + p.(qx0 mod size)) mod size)
      and q11 = p.((qy1 + p.(qx1 mod size)) mod size) in
      (* Computing vectors from the four points to the input point *)
      let tx0 = x -. x0
      and ty0 = y -. y0 in
      let tx1 = tx0 -. 1.
      and ty1 = ty0 -. 1. in
      (* Compute the dot-product between the vectors and the gradients *)
      let dot i x y = gx.(i) *. x +. gy.(i) *. y in
      let v00 = dot q00 tx0 ty0
      and v01 = dot q01 tx1 ty0
      and v10 = dot q10 tx0 ty1
      and v11 = dot q11 tx1 ty1 in
      (* Apply the chosen interpolation method to get the final value *)
      let v0 = interpolate v00 v01 tx0
      and v1 = interpolate v10 v11 tx0 in
      interpolate v0 v1 ty0

    (* fractional brownian motion *)
    let fbm octaves persistence (x, y) =
      let rec calc_sum i s =
	if i > octaves
	then
	  s
	else
          let frequency =  2.0 ** (float i)
          and amplitude =  persistence ** (float i) in
	  let s = s +. (perlin (x *. frequency, y *. frequency)) *. amplitude in
          calc_sum (succ i) s
      in
      let r = (calc_sum 1 0.) in
      clamp r (-.1.) 1.
	
  end

  (* ----------------------------------------------------------------------------- *)

  (* three dimensional perlin noise - FIX-ME: same as D1! *)
  module D3 : S with type v = float * float * float =
  struct

    type v = float * float * float

    (* gradients *)
    let gx, gy, gz = 
      let gd = Array.init size (fun _ -> Gradient.random 3) in
      Array.init size (fun i -> gd.(i).(0)),
      Array.init size (fun i -> gd.(i).(1)),
      Array.init size (fun i -> gd.(i).(2))

    (* perlin noise *)
    let perlin (x, y, z) =
      let x0 = floor x
      and y0 = floor y
      and z0 = floor z in
      (* Compute the integer positions of the eight surrounding points *)
      let qx0 = int_of_float x0
      and qy0 = int_of_float y0
      and qz0 = int_of_float z0 in
      let qx1 = succ qx0
      and qy1 = succ qy0
      and qz1 = succ qz0 in
      (* Permutate values to get indices to use with the gradient look-up tables *)
      let q000 = p.((qz0 + p.((qy0 + p.(qx0 mod size)) mod size)) mod size)
      and q001 = p.((qz0 + p.((qy0 + p.(qx1 mod size)) mod size)) mod size)
      and q010 = p.((qz0 + p.((qy1 + p.(qx0 mod size)) mod size)) mod size)
      and q011 = p.((qz0 + p.((qy1 + p.(qx1 mod size)) mod size)) mod size)
      and q100 = p.((qz1 + p.((qy0 + p.(qx0 mod size)) mod size)) mod size)
      and q101 = p.((qz1 + p.((qy0 + p.(qx1 mod size)) mod size)) mod size)
      and q110 = p.((qz1 + p.((qy1 + p.(qx0 mod size)) mod size)) mod size)
      and q111 = p.((qz1 + p.((qy1 + p.(qx1 mod size)) mod size)) mod size) in
      (* Computing vectors from the eight points to the input point *)
      let tx0 = x -. x0
      and ty0 = y -. y0
      and tz0 = z -. z0 in
      let tx1 = tx0 -. 1.
      and ty1 = ty0 -. 1.
      and tz1 = tz0 -. 1. in
      (* Compute the dot-product between the vectors and the gradients *)
      let dot a (x2,y2,z2) = gx.(a) *. x2 +. gy.(a) *. y2 +. gz.(a) *. z2 in
      let v000 = dot q000 (tx0, ty0, tz0)
      and v001 = dot q001 (tx0, ty0, tz1)
      and v010 = dot q010 (tx0, ty1, tz0)
      and v011 = dot q011 (tx0, ty1, tz1)
      and v100 = dot q100 (tx1, ty0, tz0)
      and v101 = dot q101 (tx1, ty0, tz1)
      and v110 = dot q110 (tx1, ty1, tz0)
      and v111 = dot q111 (tx1, ty1, tz1) in
      (* Do the interpolation to get the final value *)
      let v00 = interpolate v000 v001 tx0
      and v01 = interpolate v010 v011 tx0
      and v10 = interpolate v100 v101 tx0
      and v11 = interpolate v110 v111 tx0 in
      let v0 = interpolate v00 v01 ty0
      and v1 = interpolate v10 v11 ty0 in
      interpolate v0 v1 tz0


    (* fractional brownian motion *)
    let fbm octaves persistence (x, y, z) =
      let rec calc_sum i s =
	if i > octaves
	then s
	else
          let frequency =  2.0 ** (float i)
          and amplitude =  persistence ** (float i) in
	  let s = s +. (perlin (x *. frequency, y *. frequency, z *. frequency)) *. amplitude in
          calc_sum (succ i) s
      in
      let r = (calc_sum 1 0.) in
      clamp r (-.1.) 1.
  end

  (* ----------------------------------------------------------------------------- *)

  module type VEC =
  sig
    val size : int
    type 'a t 
    val get  : 'a t -> int -> 'a 
    val set  : 'a t -> int -> 'a -> unit
    val init : (int -> 'a) -> 'a t
    val dot  : 'a t -> 'a t -> 'a
    val map  : ('a -> 'b) -> 'a t -> 'b t
    val map2i : (int -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val fold_left  : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a 
    val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b 
  end


  (* n dimensional perlin noise - FIX-ME: same as D1! *)
  module Dn (V : VEC) : S with type v = float V.t = 
  struct

    type v = float V.t

    let pdim = 
      let i_2pow k = int_of_float (2.0 ** (float k))
      in
	i_2pow V.size

    (* gradients *)
    let gradients = 
      let g _ = V.init (fun i -> (random_float 2.0) -. 1.0)
      in
	Array.init size g

    (* perlin noise *)
    let perlin v =
      (* make a vector v from v1 and v2 where v[i] is chosen using the bits of k *)
      let buildbits k v1 v2 = 
	let choose i a b = 
	  if (k land i) = 1 then b else a
	in
	V.map2i choose v1 v2
      in
      let x = V.map floor v
      in
      (* Compute the integer positions of the (2**n) surrounding points *)
      let q0 : int V.t = V.map int_of_float x
      in
      let q1 = V.map succ q0
      in
      (* Permutate values to get indices to use with the gradient look-up tables *)
      let qp : int array = 
	Array.init pdim (
	  fun k -> let v = buildbits k q0 q1
		   in
		   let pk acc vi = p.((vi + acc) mod size)
		   in
		   V.fold_left pk 0 v
	)
      in
      (* Computing vectors from the (2**n) points to the input point *)
      let t0 = V.map2i (fun _ a b -> a -. b) v x
      in
      let t1 = V.map  (fun a -> a -. 1.) t0
      in
      (* Compute the dot-product between the vectors and the gradients *)
      let vk =
	let dot a v = V.dot gradients.(a) v
	in
	Array.init pdim (
	  fun k -> 
	    let vi = buildbits k t0 t1
	    in
	    dot qp.(k) vi
	)
      in
      (* Do the 'n-cubic' interpolation to get the final value 
	 [ 0 ; 1 ; 0 ; 1 ; 0 ; 1 ; 0 ; 1 ] 1 * (2 * i),  1 * (2 * i + 1) -> 1 * (2 * i)
	 [ 0 ; _ ; 1 ; _ ; 0 ; _ ; 1 ; _ ] 2 * (2 * i),  2 * (2 * i + 1) -> 2 * (2 * i)
	 [ 0 ; _ ; _ ; _ ; 1 ; _ ; _ ; _ ] 4 * (2 * i),  4 * (2 * i + 1) -> 4 * (2 * i)
	 [ ...                           ] (2**i) * (2 * i), (2**i) * (2 * i + 1) 
      *)
      let rec ncubic vk t k p d =
	if d = 1
	then interpolate vk.(0) vk.(p) (V.get t k)
	else (
	  for i = 0 to pred d do
	    let k0 = p * (2 * i) 
	    and k1 = p * (2 * i + 1)
	    in
	    vk.(k0) <- interpolate vk.(k0) vk.(k1) (V.get t k)
	  done;
	  ncubic vk t (succ k) (p * 2) (d / 2)
	)
      in
      ncubic vk t0 0 1 pdim

    (* fractional brownian motion *)
    let fbm octaves persistence v =
      let rec calc_sum i s =
	if i < octaves
	then s
	else
          let frequency =  2.0 ** (float i)
          and amplitude =  persistence ** (float i) in
	  let v' = V.map (fun vi -> vi *. frequency) v in
          calc_sum (succ i) (s +. (perlin v') *. amplitude)
      in
      let r = (calc_sum 1 0.) in
      clamp r (-.1.) 1.


  end
    
    
end

