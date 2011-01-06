
module type INTERPOLATE =
sig 
  type t
  val f : t -> t -> t -> t 
end

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

module Make (Interpolate : INTERPOLATE with type t = float) = 
struct

  let persistence   = 1.
  let octave_number = 6

  let clamp v vmin vmax = 
    max (min v vmax) vmin
      
  module D2 =
  struct
    let size = 256

    (* permutation table *)
    let p = 
      let p = Array.init size (fun i -> i)
      in
	for i = 0 to pred size do
	  let j = Random.int size in 
	  let s = p.(i) in
	    p.(i) <- p.(j);
	    p.(j) <- s
	done;
	p

    (* gradients *)
    let gx, gy = 
      Array.init size (fun i -> (Random.float 2.0) -. 1.0),
      Array.init size (fun i -> (Random.float 2.0) -. 1.0)
	
    let gen (x, y) =
      let x0 = floor x
      and y0 = floor y
      in
	(* Compute the integer positions of the four surrounding points *)
      let qx0 = int_of_float x0
      and qy0 = int_of_float y0
      in
      let qx1 = succ qx0
      and qy1 = succ qy0
      in
	(* Permutate values to get indices to use with the gradient look-up tables *)
      let q00 = p.((qy0 + p.(qx0 mod size)) mod size)
      and q01 = p.((qy0 + p.(qx1 mod size)) mod size)
      and q10 = p.((qy1 + p.(qx0 mod size)) mod size)
      and q11 = p.((qy1 + p.(qx1 mod size)) mod size)
      in
	(* Computing vectors from the four points to the input point *)
      let tx0 = x -. x0
      and ty0 = y -. y0
      in
      let tx1 = tx0 -. 1.
      and ty1 = ty0 -. 1.
      in
	(* Compute the dot-product between the vectors and the gradients *)
      let v00 = gx.(q00) *. tx0 +. gy.(q00) *. ty0
      and v01 = gx.(q01) *. tx1 +. gy.(q01) *. ty0
      and v10 = gx.(q10) *. tx0 +. gy.(q10) *. ty1
      and v11 = gx.(q11) *. tx1 +. gy.(q11) *. ty1
      in
	(* Do the bi-cubic interpolation to get the final value *)
      let v0 = Interpolate.f v00 v01 tx0
      and v1 = Interpolate.f v10 v11 tx0
      in
	Interpolate.f v0 v1 ty0
	  (*
      let wx = (3. -. 2. *. tx0) *. tx0 *. tx0  
      and wy = (3. -. 2. *. ty0) *. ty0 *. ty0
      in
      let v0 = v00 -. wx *. (v00 -. v01) (* v0 = cubic(v00, v01, tx0) *)
      and v1 = v10 -. wx *. (v10 -. v11) (* v1 = cubic(v10, v11, tx0) *)
      in
	v0 -. wy *. (v0 -. v1)    (* v  = cubic(v0, v1, ty0) *)
	  *)

    let perlin (x, y) =
      let p = persistence
      and n = octave_number
      in
      let rec calc_sum i s =
	if i = n 
	then s
	else
          let frequency =  2.0 ** (float i)
          and amplitude =  p ** (float i)
	  in
	  let s = s +. (gen (x /. frequency, y /. frequency)) *. amplitude
	  in
            calc_sum (succ i) s
      in
      let r = (calc_sum 0 0.)
      in
	clamp r (-.1.) 1.
  end
	
end

