(*
  a perlin noise library

  Copyright (C) 2010,2011,2012  Didier Cassirame

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

  val random_int   : int -> int
  val random_float : float -> float
  val curve        : float -> float
  val psize        : int

end

module type S =
sig

  type v 

  val perlin : v -> float

  val fbm    : int -> float -> v -> float

end

module Make(Config: CONFIG) = 
struct

  let random_int x = Config.random_int x
  let random_float x = Config.random_float x
  let curve = Config.curve

  let b  = Config.psize
  let n  = float 0x1000

  let p = Array.make (b + b + 2) 0
  let g1 = Array.make (b + b + 2) 0.
  let g2 = Array.make ((b + b + 2) * 2) 0.
  let g3 = Array.make ((b + b + 2) * 3) 0. 

  let clamp v vmin vmax = 
    max (min v vmax) vmin

  let normalize2 g i = 
    let l = sqrt (g.(i) *. g.(i) +. g.(i+1) *. g.(i+1)) in
    g.(i) <- g.(i) /. l;
    g.(i+1) <- g.(i+1) /. l

  let normalize3 g i = 
    let l = sqrt (g.(i) *. g.(i) +. g.(i+1) *. g.(i+1) +. g.(i+2) *. g.(i+2)) in
    g.(i) <- g.(i) /. l;
    g.(i+1) <- g.(i+1) /. l;
    g.(i+2) <- g.(i+2) /. l

  let init () =
    let frandom () = 
      (random_float 2.0) -. 1.
    in
    for i = 0 to pred b do 
      p.(i) <- i;
      g1.(i) <- frandom ();
      for j = 0 to 1 do
	g2.(i*2 + j) <- frandom ()
      done;
      normalize2 g2 i;
      for j = 0 to 2 do
	g3.(i*3 + j) <- frandom ()
      done;
      normalize3 g3 i;
    done;
    for i = pred b downto 1 do
      let k = p.(i) 
      and j = random_int b in
      p.(i) <- p.(j);
      p.(j) <- k
    done;
    for i = 0 to b + 1 do 
      p.(b + i) <- p.(i);
      g1.(b + i) <- g1.(i);
      for j = 0 to 1 do
	g2.((i+b)*2+j) <- g2.(i*2+j)
      done;
      for j = 0 to 2 do
	g3.((i+b)*3+j) <- g3.(i*3+j)
      done
    done

  let lerp t a b = a +. t *. (b -. a)

  let _ = init ()
    
  module D1 : S with type v = float =
  struct
    
    type v = float

    let perlin x =
      let t = x +. n in
      let bx0 = (truncate t) mod b
      and rx0 = t -. (floor t) in
      let bx1 = (succ bx0) mod b
      and rx1 = rx0 -. 1. in
      let sx = curve rx0 in
      let u = rx0 *. (g1.(p.(bx0)))
      and v = rx1 *. (g1.(p.(bx1))) in
      Printf.printf "%d,%d,%f,%f,%f,%f,%f\n" bx0 bx1 rx0 rx1 u v sx;
      lerp sx u v

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

  module D2 : S with type v = float * float = 
  struct

    type v = float * float

    let perlin (x,y) = 
      let tx = x +. n
      and ty = y +. n in
      let bx0 = (truncate tx) mod b
      and rx0 = tx -. (floor tx)
      and by0 = (truncate ty) mod b
      and ry0 = ty -. (floor ty) in
      let bx1 = (succ bx0) mod b
      and rx1 = rx0 -. 1.
      and by1 = (succ by0) mod b 
      and ry1 = ry0 -. 1. in
      let i,j  = p.(bx0), p.(bx1) in
      let b00 = p.(i+by0)
      and b10 = p.(j+by0)
      and b01 = p.(i+by1)
      and b11 = p.(j+by1) in
      let sx = curve rx0
      and sy = curve ry0 in
      let dot2 o rx ry = rx *. g2.(o+0) +. ry *. g2.(o+1) in
      let u1 = dot2 b00 rx0 ry0
      and v1 = dot2 b10 rx1 ry0 
      and u2 = dot2 b01 rx0 ry1
      and v2 = dot2 b11 rx1 ry1 in
      lerp sy (lerp sx u1 v1) (lerp sx u2 v2)

    (* fractional brownian motion *)
    let fbm octaves persistence (x, y) =
      let rec calc_sum i s =
	if i > octaves
	then s
	else
          let frequency =  2.0 ** (float i)
          and amplitude =  persistence ** (float i) in
	  let s = s +. (perlin (x *. frequency, y *. frequency)) *. amplitude in
          calc_sum (succ i) s
      in
      let r = (calc_sum 1 0.) in
      clamp r (-.1.) 1.


  end

  module D3  : S with type v = float * float * float = 
  struct

    type v = float * float * float

    let perlin (x,y,z) =
      let tx = x +. n 
      and ty = y +. n
      and tz = z +. n in
      let bx0 = (truncate tx) mod b
      and rx0 = tx -. (floor tx)
      and by0 = (truncate ty) mod b
      and ry0 = ty -. (floor ty)
      and bz0 = (truncate tz) mod b 
      and rz0 = tz -. (floor tz) in
      let bx1 = (succ bx0) mod b
      and rx1 = rx0 -. 1. 
      and by1 = (succ by0) mod b 
      and ry1 = ry0 -. 1. 
      and bz1 = (succ by0) mod b 
      and rz1 = rz0 -. 1. in
      let i,j  = p.(bx0), p.(bx1) in
      let b00 = p.(i+by0)
      and b10 = p.(j+by0)
      and b01 = p.(i+by1)
      and b11 = p.(j+by1) 
      in
      let sx = curve rx0
      and sy = curve ry0 
      and sz = curve rz0 in

      let dot3 o rx ry rz = rx *. g3.(o+0) +. ry *. g3.(o+1) +. rz *. g3.(o+2) in
      let u1 = dot3 (b00+bz0) rx0 ry0 rz0
      and v1 = dot3 (b10+bz0) rx1 ry0 rz0
      and u2 = dot3 (b01+bz0) rx0 ry1 rz0
      and v2 = dot3 (b11+bz0) rx1 ry1 rz0 in
      let u3 = dot3 (b00+bz1) rx0 ry0 rz1
      and v3 = dot3 (b10+bz1) rx1 ry0 rz1
      and u4 = dot3 (b01+bz1) rx0 ry1 rz1
      and v4 = dot3 (b11+bz1) rx1 ry1 rz1 in

      let a = lerp sy (lerp sx u1 v1) (lerp sx u2 v2) 
      and b = lerp sy (lerp sx u3 v3) (lerp sx u4 v4) in
      lerp sz a b

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

(*
  module type VEC =
  sig
    val size : int
    type 'a t
    val get : 'a t -> int -> 'a
    val set : 'a t -> int -> 'a -> unit
    val init : (int -> 'a) -> 'a t
    val dot  : 'a t -> 'a t -> 'a
    val map  : ('a -> 'b) -> 'a t -> 'b t
    val dot : 'a t -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val map2i : (int -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val fold_left  : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a 
    val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b 
  end
 
  (* n dimensional perlin noise - FIX-ME: same as D1! *)
  module Dn (V : VEC) : S with type v = float V.t = 
  struct

    let pdim = 
      let i_2pow k = int_of_float (2.0 ** (float k)) in
      i_2pow V.size
	
    (* gradients *)
    let gradients = 
      let g _ = V.init (fun i -> (random_float 2.0) -. 1.0) in
      Array.init b g

    type v = float V.t
	
     (* perlin noise *)
    let perlin v =
      (* make a vector v from v1 and v2 where v[i] is chosen using the bits of k *)
      let buildbits k v1 v2 = 
	let choose i a b = 
          if (k land i) = 1 then b else a
	in
	V.map2i choose v1 v2
      in
      let x = V.map floor v in
      (* Compute the integer positions of the (2**n) surrounding points *)
      let q0 : int V.t = V.map int_of_float x in
      let q1 = V.map succ q0 in
      (* Permutate values to get indices to use with the gradient look-up tables *)
      let qp : int array = 
	Array.init pdim (fun k -> 
	  let v = buildbits k q0 q1 in
          let pk acc vi = p.((vi + acc) mod size) in
          V.fold_left pk 0 v
	)
      in
      (* Computing vectors from the (2**n) points to the input point *)
      let t0 = V.map2i (fun _ a b -> a -. b) v x in
      let t1 = V.map  (fun a -> a -. 1.) t0 in
      (* Compute the dot-product between the vectors and the gradients *)
      let vk =
	let dot a v = V.dot gradients.(a) v
	in
	Array.init pdim (fun k -> 
          let vi = buildbits k t0 t1 in
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
            vk.(k0) <- lerp vk.(k0) vk.(k1) (V.get t k)
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
	
  end (* module Dn *)
    *)

    
end
