
type t = {
  scale : float;
  field : float array array;
}

let dims h =
  Array.length h.field.(0), Array.length h.field

let get h x z = 
  let y = h.field.(z).(x) in 
  float x, y,float z

(* compute the normal between points (x z) ... of the heightfield hf *)
let normal hf x z x' z' x'' z'' = 
  let x,   y,   z   = get hf x   z
  and x'  ,y',  z'  = get hf x'  z'
  and x'', y'', z'' = get hf x'' z'' in
  let x1, y1, z1 = x' -. x  , y' -. y   , z' -. z
  and x2, y2, z2 = x' -. x'', y' -. y'' , z' -. z'' in
  let zn = y1 *. x2 -. x1 *. y2
  and xn = z1 *. y2 -. y1 *. z2
  and yn = z2 *. x1 -. x2 *. z1 in
  let m = 1. /. (sqrt (xn *. xn +. yn *. yn +. zn *. zn)) in
  xn *. m, yn *. m, zn *. m

let center hf x z x' z' x'' z'' = 
  let x,   y,   z   = get hf x   z
  and x'  ,y',  z'  = get hf x'  z'
  and x'', y'', z'' = get hf x'' z'' in
  (x +. x' +. x'') /. 3., (y +. y' +. y'') /. 3., (z +. z' +. z'') /. 3.

let dunes width height =
  let module NConfig : (Noise.CONFIG) = 
      struct
	let interpolate = Interpolate.quintic
	let psize       = 256
      end 
  in
  let module N = Noise.Make(NConfig) in
  let perlin = N.D2.perlin in
  (* At round values, the perlin gradients are null, so we'll get a pure 
     flat heightfield if we use the integer coords of each point. Instead, 
     we concentrate on a small patch of the perlin space.
  *) 
  let hscale = 3. /. (float height)
  and wscale = 3. /. (float width) in
  { 
    scale = 1.;
    field = 
      Array.init height (fun y -> Array.init width (fun x ->
	let x = (float x) *. wscale
	and y = (float y) *. hscale in
	perlin (x,y) *. 2.
      ));
  }
    
let mountains width height =
  let module NConfig : (Noise.CONFIG) = 
      struct
	let interpolate = Interpolate.quintic
	let psize       = 256
      end 
  in
  let module N = Noise.Make(NConfig) in
  let module NConfMask = 
      struct 
	let interpolate = Interpolate.quintic 
	let psize = 256 
      end in
  let module NMask = Noise.Make(NConfMask) in
  (* We combine 2 noises functions spanning on the same random space, but using
     different numbers of octave to get a smoother result for the lowest values
     and a more spiky one for the peeks.
  *)
  let persistence = 1. /. (sqrt 2.0) in
  let combine l h x y =
    let mask = 15. *. (
      NMask.D2.fbm 2 persistence (
	(float x) *. 0.005,
	(float y) *. 0.010
      )
    ) in
    max (h *. mask) (2.1 *. l)
  in
  let perlin_low  = N.D2.fbm 2 persistence
  and perlin_high = N.D2.fbm 6 persistence in
  {
    scale = 1.;
    field = Array.init height (fun y -> Array.init width (fun x ->
      let scale_low (x,y) = 
	(float x) *. 0.007,
	(float y) *. 0.015
      and scale_high (x,y) = 
	(float x) *. 0.002,
	(float y) *. 0.004
      in
      let low  = perlin_low (scale_low (x,y))
      and high = 0.5 +. 3. *. perlin_high (scale_high (x,y)) in
      (combine low high x y) *. 2.)
    );
  }
