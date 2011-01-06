module Interpolate  =
struct

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

end

module type INTERPOLATE =
sig 
  type t
  val f : t -> t -> t -> t 
end

(* integer power function *)
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

module Noise (Interpolate : INTERPOLATE with type t = float) = 
struct 

  module Int = Int32

  type int          = Int32.t

  let ( land )  = Int.logand (* logical and *)
  let ( lxor )  = Int.logxor
  let ( lor )   = Int.logor
  let ( lsl )   = Int.shift_left
  let ( lsr )   = Int.shift_right_logical
  let ( +  )    = Int.add
  let ( - )     = Int.sub
  let ( * )     = Int.mul
  let ( / )     = Int.div
  let of_int    = Int.of_int
  let to_int    = Int.to_int
  let of_float  = Int.of_float
  let ( float ) = Int.to_float
  let pred      = Int.pred
  let succ      = Int.succ
  let zero      = Int.zero
  let one       = Int.one
  let two       = one + one
  let max_int   = Int.max_int
  let ipow a b  = of_int (ipow (to_int a) (to_int b))

  let persistence   = (of_int 1)
  let octave_number = (of_int 8)

  let clamp v vmin vmax = 
    max (min v vmax) vmin

  module D1 =
  struct 

    let primes i =
      match i with
	  _ -> (of_int 15731), (of_int 789221), of_int 1376312589, 1073741824.0

    let gen i v =
      let k1, k2, k3, k4 = primes i in
      let x   = ((v lsl 13) lxor v) in
      let r   = (x * (x * x * k1 + k2 ) + k3) land (max_int lsr 1)
      in
	1.0 -. ((float)r /. k4)

    let smooth i x = 
      ((gen i x) /. 2.) +. ((gen i (pred x)) /. 4.) +. ((gen i (succ x)) /. 4.)

    let interpolated i x =
      let noise = smooth in
      let ix = of_float x in
      let fx = x -. ((float)ix) in
      let v1 = noise i ix
      and v2 = noise i (ix + one)
      in
	Interpolate.f v1 v2 fx

    let perlin x = 
      let p = persistence
      and n = octave_number
      in
      let rec calc_sum i s =
	if i = zero
	then s 
	else
          let frequency = (float)(ipow two i)
          and amplitude = (float)(ipow p i)
	  in
            calc_sum (pred i) (s +. ((interpolated i (x *. frequency)) *. amplitude))
      in
	calc_sum n 0.

  end (* module D1 *)

  module D2 =
  struct

    let gen i (x, y) = 
      let n = x + y * (of_int 57) 
      in 
	D1.gen i n

    (* smoothing the (x,y) corresponding value with values surrounding it 
       this code seems to imply that values lie necessarily on a integral 
       grid *)
    let smooth i (x, y) = 
      let corners = 
	((gen i (pred x, pred y)) +. 
	   (gen i (succ x, pred y)) +. 
	   (gen i (pred x, succ y)) +. 
	   (gen i (succ x, succ y))
	) /. 16.0
      and sides   = 
	((gen i (pred x, y)) +. 
	   (gen i (succ x, y)) +. 
	   (gen i (x, pred y)) +. 
	   (gen i (x, succ y)) 
	) /. 8.0
      and center  =  
	(gen i (x, y)) /. 4.0
      in
	corners +. sides +. center


    (* compute the interpolated value at (x,y) 
       where x and y are floating point coords
       we get the integral values of the coords
       and compute the surrounding values
       then we interpolate according to the decimals
       of (x,y)
       why use smooth to compute the values at (x,y)
       since we interpolate anyway?
    *)
    let interpolated i (x, y) = 
      let noise = gen in
      let ix  = of_float x 
      and iy  = of_float y 
      in
      let fx  = x -. ((float)ix)
      and fy  = y -. ((float)iy)
      in
      let v1 = noise i (ix     , iy     )
      and v2 = noise i (succ ix, iy     )
      and v3 = noise i (ix     , succ iy)
      and v4 = noise i (succ ix, succ iy)
      in
      let i1 = Interpolate.f v1 v2 fx
      and i2 = Interpolate.f v3 v4 fx
      in Interpolate.f i1 i2 fy

    (* we compute values at different octaves for the same point
       and sum up the results.
       Can we normalize this so that we may map it easily to some 
       other kind of dataset?
    *)
    let perlin (x, y) =
      let p = persistence
      and n = octave_number
      in
      let rec calc_sum i s =
	if i = n 
	then s 
	else
          let frequency = (float)(ipow two i)
          and amplitude = (float)(ipow p i)
	  in
            calc_sum (succ i) (s +. ((interpolated i ((x *. frequency),(y *. frequency))) *. amplitude))
      in
	clamp (calc_sum zero 0.) 1.0 (-.1.0)

  end (* module D2 *)

(*
  module D3 =
  struct 

    let gen3d x y z = 
      let n = x +. y *.57 + z *. 103
      in
	D1.gen n

  end (* module D3 *)
*)
end

