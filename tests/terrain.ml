module HeightField =
struct


  type t = {
    scale : float;
    field : float array array;
  }

  let dims h =
    Array.length h.field.(0), Array.length h.field

  let get h x y = h.field.(y).(x)


  let render (hf : t) =
    let w, h = dims hf in
    let hw = w / 2
    and hh = h / 2 in
    let point x z =
      let y = get hf x z in
      GlDraw.vertex3 (hf.scale *. (float (x - hw)),
		      hf.scale *. 5. *. y,
		      hf.scale *. (float (z - hh))
      )
    in
    GlDraw.polygon_mode `both `line;
    GlDraw.color (1.0,1.0,1.0);
    for z = 0 to h - 2 do
      GlDraw.begins `triangle_strip;
      for x = 0 to pred w do
	point x z;
	point x (succ z)
      done;
      GlDraw.ends ()
    done


let dune ?(scale = 8) width height =
  let module NConfig : (Noise.CONFIG) = 
      struct
	(* we choose the linear interpolation, as a better one doesn't seem to bring much difference
	   in this case 
	*)
	let interpolate = Interpolate.quintic
	let psize       = 256
      end 
  in
  let module N = Noise.Make(NConfig) in
  let module HF = HeightField in
  (*
    let scale = 
    let rec comp x r = 
    if x = 1 then r else comp (pred x) (r * 2)
    in
    comp def 2
    in
  *)
  (* we combine 2 noises functions spanning on the same random space, but using
     different numbers of octave to get a smoother result for the lowest values
     and a more spiky one for the peeks.
  *)
  let persistence = 1. /. (sqrt 2.0) in
  let perlin = N.D2.gen in
  (* At round values, the perlin gradients are null, so we'll get a pure 
     flat heightfield if we use the integer coords of each point. Instead, 
     we concentrate on a small patch of the perlin space.
  *) 
  { 
    scale = 1. /. float scale;
    field = 
      Array.init height (fun y -> Array.init width (fun x ->
	let x = (float x) /. float (height / scale)
	and y = (float y) /. float (width / scale) in
	perlin (x,y)
      ));
  }

let mountains ?(scale = 8) width height =
  let module NConfig : (Noise.CONFIG) = 
      struct
	(* we choose the linear interpolation, as a better one doesn't seem to bring much difference
	   in this case 
	*)
	let interpolate = Interpolate.linear 
	let psize       = 256
      end 
  in
  let module N = Noise.Make(NConfig) in
    (*
      let scale = 
      let rec comp x r = 
      if x = 1 then r else comp (pred x) (r * 2)
      in
      comp def 2
      in
    *)
    (* we combine 2 noises functions spanning on the same random space, but using
       different numbers of octave to get a smoother result for the lowest values
       and a more spiky one for the peeks.
    *)
  let persistence = 1. /. (sqrt 2.0) in
  let perlin_low = N.D2.perlin 2 persistence
  and perlin_high = N.D2.perlin 7 persistence in
    (* At round values, the perlin gradients are null, so we'll get a pure 
       flat heightfield if we use the integer coords of each point. Instead, 
       we concentrate on a small patch of the perlin space.
    *) 
  { 
    scale = 1. /. float scale;
    field = 
      Array.init height (fun y -> Array.init width (fun x ->
	let x = (float x) /. float (height / scale)
	and y = (float y) /. float (width / scale) in
	let low  = perlin_low (x,y)
	and high = perlin_high (x,y) in 
	(max (max low high) (-0.5)) *. 2.0 +. 2.0
      ));
  }

