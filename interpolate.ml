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
