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

open Sdl
open Sdlvideo
open Sdlevent
open Sdlloader
open Sdlkey
open Sdlmouse
open Sdlgl
open Noise
open Gmaths


(* ---------------------------------------------------------------------- *)

let usleep () = ignore (Unix.select [] [] [] 0.01 )

let debug_vec s v =
  prerr_string s;  Printf.fprintf stderr "[| %f ; %f ; %f |]\n" v.(0) v.(1) v.(2)

let debug_float s v = 
  prerr_string s; prerr_float v; prerr_newline ()

(* ---------------------------------------------------------------------- *)

(* drawing list based grid *)
class grid_list =
object(self)
  
  val mutable grid = None

  method private build () = 
    let grid_step = 10 in
    let l = GlList.create `compile 
    in
    GlList.begins l ~mode:`compile;
    for j=(0-20) to 20 do 
      GlDraw.begins `quad_strip;
      for i=(0-20) to 20 do
	let fi = float_of_int (i*grid_step) in
	GlDraw.vertex ~x:(fi) ~z:(float_of_int ((j+1)*grid_step))  ~y:(0.) ();
	GlDraw.vertex ~x:(fi) ~z:(float_of_int (j*grid_step))      ~y:(0.) ()
      done;
      GlDraw.ends ()
    done;
    GlList.ends ();
    grid <- Some l
      
  method render () : unit =
    match grid with
	Some g -> GlList.call g
      | _      -> ()

  initializer (
    self#build ()
  )

end

(* vertex array based grid *)
class grid =
object(self)
  
  val mutable grid = None

  val mutable count = 0

  method private build () = 
    let grid_step = 10 in
    let c         = 20 in                  (* number of horiz. lines on each side of origin *)
    let ltotal    = c * 2 in               (* total number of horiz lines excepted origin *)
    let vcount    = (ltotal + 1) * 4 in    (* total number of vertices including origin *)
    let total     = vcount * 3 in          (* total number of coord components *)
    (* geometry *)
    let rg = Raw.create_static `double total in
    Gc.finalise Raw.free_static rg;
    let coord k = float_of_int ((k - c) * grid_step) in
    (* x lines *)
    for i = 0 to ltotal do 
      let k  = i * 6 in
      Raw.sets_float rg k [|
	coord i ; 0. ; coord 0;
	coord i ; 0. ; coord ltotal
			  |]
    done;
    (* z lines *)
    for i = 0 to ltotal do
      let k  = (1 + ltotal + i) * 6 in
      Raw.sets_float rg k [|
	coord ltotal ; 0. ; coord i;
	coord 0 ; 0. ; coord i
			  |]
    done;
    (* indices *)
    let ri = Raw.create `uint vcount in
    for i = 0 to succ (ltotal * 2) do
      let k = i * 2 in
      Raw.sets ri ~pos:k [| k ; k + 1 |]
    done;
    count <- vcount;
    grid  <- Some (rg,ri);
    GlArray.vertex `three (rg);

  method refresh () =
    match grid with
      | Some (rg,ri) -> (
	GlArray.vertex `three rg;
      )
      | _            -> ()

  method render () : unit =
    match grid with
	Some (rg,ri) -> (
	  GlArray.enable `vertex;
	  GlArray.vertex `three rg;
	  GlArray.draw_elements `lines ~count ri;
	  GlArray.disable `vertex
	)
      | _           -> ()

  initializer (
    self#build ()
  )

end

class grid_one =
object(self)
  
  val mutable grid = None

  val mutable count = 0

  method private build () = 
    let grid_step = 10 in
    let rg = Raw.create_static `double (12) in
    let k  = 0 in
    (* geometry *)
    Raw.sets_float rg k
      [|
	0. ; 0. ; float_of_int (0);
	0. ; 0. ; float_of_int (grid_step);
	float_of_int (2*grid_step) ; 0. ; float_of_int (grid_step);
	float_of_int (2*grid_step) ; 0. ; 0.;
      |];
    (* indices *)
    let ri = Raw.of_array
      [|
	0; 1; 
	2; 3; 0
      |] `uint in
    count <- 5;
    grid  <- Some (rg,ri)
    
  method render () : unit =
    match grid with
	Some (_,g) -> (
	  GlArray.enable `vertex;
	  GlArray.draw_elements `line_strip ~count g;
	  GlArray.disable `vertex
	)
      | _      -> ()

  initializer (
    self#build ()
  )

end

class light num =
object(self)

  val mutable theta = 0.0

  val dtheta = acos (-1.) /. 360.

  val twopi = acos (-1.) *. 2.

  val id =
    match num with
      | 1 -> `light1
      | 2 -> `light2
      | 3 -> `light3
      | 4 -> `light4
      | 5 -> `light5
      | 6 -> `light6
      | 7 -> `light7
      | _ -> `light0

  val mutable position = 100., 100., 0., 1.0

  val mutable ambient = 0.5, 0.5, 0.5, 1.0

  val mutable diffuse = 0.5, 0.5, 0.5, 1.0

  val mutable specular = 0.0, 0.0, 0.0, 0.0

  method private update_position () =
    position <- 100. *. (cos theta), 100., 100. *. (sin theta), 1.0;
    theta <- theta +. dtheta;
    if theta > twopi then theta <- theta -. twopi


  method render () : unit = 
    Gl.enable id;
    GlDraw.point_size 20.0;
    GlDraw.color (1.,1.,1.);
    GlDraw.begins `points;
    GlDraw.vertex4 position;
    GlDraw.ends ();
    GlDraw.point_size 1.0;
    GlLight.light num (`position (position));
    GlLight.light num (`ambient (ambient));
    GlLight.light num (`diffuse (diffuse));
    GlLight.light num (`specular (specular));
    self#update_position ()
    

end

(* ---------------------------------------------------------------------- *)

class type hf_render =
object
  
  method render_normal : unit -> unit

  method render : unit -> unit

end

class hf_direct_render hf =
  let w,h = HeightField.dims hf in
object(self)

  method render_normals () = 
    let render_normal hf x z x' z' x'' z'' =
      let xn,yn,zn = HeightField.normal hf x z x' z' x'' z''
      and x,y,z = HeightField.center hf x z x' z' x'' z'' in
      Gl.disable `lighting;
      GlDraw.color (0.,0.,1.);
      GlDraw.begins `lines;
      GlDraw.vertex3 (x, y, z);
      GlDraw.vertex3 (x +. xn, y +. yn, z +. zn);
      GlDraw.ends ();
      GlDraw.point_size 2.0;
      GlDraw.begins `points;
      GlDraw.vertex3 (x +. xn, y +. yn, z +. zn);
      GlDraw.ends ();
      GlDraw.point_size 1.0
    in
    for z = 0 to h - 2 do
      for x = 1 to w - 2 do
	render_normal hf (pred x) z (pred x) (succ z) x (succ z);
	render_normal hf (pred x) z x (succ z) x z;
      done;
    done

  method render () =
    for z = 0 to h - 2 do
      GlDraw.begins `triangle_strip;
      GlDraw.vertex3 (HeightField.get hf 0 (succ z));
      GlDraw.vertex3 (HeightField.get hf 0 z);
      for x = 1 to w - 2 do
	GlDraw.vertex3 (HeightField.get hf x (succ z));
	GlDraw.normal3 (HeightField.normal hf (pred x) z (pred x) (succ z) x (succ z));
	GlDraw.vertex3 (HeightField.get hf x z);
	GlDraw.normal3 (HeightField.normal hf (pred x) z x (succ z) x z);
      done;
      GlDraw.ends ()
    done
      
end

let hf_direct w h = 
  let hf = HeightField.mountains w h in
  new hf_direct_render hf

(* ------------------------ *)

class hf_array_render hf r n =
  let w,h = HeightField.dims hf in
object(self)

  method render_normals () = 
    let render_normal hf x z x' z' x'' z'' =
      let xn,yn,zn = HeightField.normal hf x z x' z' x'' z''
      and x,y,z = HeightField.center hf x z x' z' x'' z'' in
      Gl.disable `lighting;
      GlDraw.color (0.,0.,1.);
      GlDraw.begins `lines;
      GlDraw.vertex3 (x, y, z);
      GlDraw.vertex3 (x +. xn, y +. yn, z +. zn);
      GlDraw.ends ();
      GlDraw.point_size 2.0;
      GlDraw.begins `points;
      GlDraw.vertex3 (x +. xn, y +. yn, z +. zn);
      GlDraw.ends ();
      GlDraw.point_size 1.0
    in
    for z = 0 to h - 2 do
      for x = 0 to w - 2 do
	render_normal hf x z (succ x) (succ z) (succ x) z;
	render_normal hf x z x (succ z) (succ x) (succ z);
      done;
    done

  method refresh () =
    GlArray.vertex `three r;
    GlArray.normal n

  method render () : unit =
    GlArray.enable `vertex;
    GlArray.enable `normal;
    self#refresh ();
    for i = 0 to (w - 2) do 
      GlArray.draw_arrays `triangle_strip ~first:(i * h * 2) ~count:(h * 2);
    done;
    GlArray.disable `vertex;
    GlArray.disable `normal

  initializer (
    self#refresh ()
  )

end

let hf_array w h =
  let hf = HeightField.mountains w h in
  (* triangle strip vertices : 2 vertices * width * (height - 1) *)
  let vcount = (h - 1) * 2 * w in
  (* double : 3 components * strip count *)
  let r = Raw.create_static `double (vcount * 3) in
  Gc.finalise Raw.free_static r;
  for z = 0 to h - 2 do
    for x = 0 to w - 1 do
      let vx,vy,vz    = HeightField.get hf x (succ z)
      and vx',vy',vz' = HeightField.get hf x z in
      Raw.sets_float r ((z * w + x) * 6) [| vx; vy; vz ; vx'; vy'; vz' |];
    done;
  done;
  (* normals : 2 per vertex *)
  let ncount = (pred h) * w * 2 in
  let n = Raw.create_static `double (ncount * 3) in
  Gc.finalise Raw.free_static n;
  for z = 0 to h - 2 do
    for x = 0 to w - 2 do
      let xn ,yn ,zn  = HeightField.normal hf x z (succ x) (succ z) (succ x) z
      and xn',yn',zn' = HeightField.normal hf x z x (succ z) (succ x) (succ z) in
      Raw.sets_float n ((z * w + x + 1) * 6) [| xn; yn; zn; xn'; yn'; zn' |]
    done;
  done;
  new hf_array_render hf r n

(* ------------------------ *)

class hf_element_render hf r n e = 
  let w,h = HeightField.dims hf in
object(self)

  val count = (pred w) * 2

  method render_normals () = 
    let render_normal hf x z x' z' x'' z'' =
      let xn,yn,zn = HeightField.normal hf x z x' z' x'' z''
      and x,y,z = HeightField.center hf x z x' z' x'' z'' in
      Gl.disable `lighting;
      GlDraw.color (0.,0.,1.);
      GlDraw.begins `lines;
      GlDraw.vertex3 (x, y, z);
      GlDraw.vertex3 (x +. xn, y +. yn, z +. zn);
      GlDraw.ends ();
      GlDraw.point_size 2.0;
      GlDraw.begins `points;
      GlDraw.vertex3 (x +. xn, y +. yn, z +. zn);
      GlDraw.ends ();
      GlDraw.point_size 1.0
    in
    for z = 0 to h - 2 do
      for x = 0 to w - 2 do
	render_normal hf x z (succ x) (succ z) (succ x) z;
	render_normal hf x z x (succ z) (succ x) (succ z)
      done;
    done

  method refresh () =
    GlArray.vertex `three r;
    GlArray.normal n

  method render ()  =
    Gl.disable `lighting;
    GlDraw.color (1.,1.,1.);

    GlArray.enable `vertex;
    GlArray.enable `normal;
    self#refresh ();
    Array.iter (fun a -> 
      GlArray.draw_elements `triangle_strip ~count a
    ) e;
    GlArray.disable `vertex;
    GlArray.disable `normal

  initializer (
    self#refresh ()
  )
   
end

let hf_elements w h =
  let hf = HeightField.mountains w h in
  let count = (pred w) * 2 in
  (* vertices : 3 components * width * height *)
  let r = Raw.create_static `double (h * w * 3) in
  Gc.finalise Raw.free_static r;
  for z = 0 to h - 1 do
    for x = 0 to w - 1 do
      try 
	let vx,vy,vz = HeightField.get hf x z in
	Raw.sets_float r ((z * w + x) * 3) [| vx; vy; vz |];
      with _ -> (Printf.fprintf stderr "vertices : %d %d" x z; exit 1)
    done;
  done;
  (* normals : 3 components *)
  let ccount = (pred h) * w * 3 in
  let n = Raw.create_static `double ccount in
  Gc.finalise Raw.free_static n;
  for z = 0 to h - 2 do
    for x = 0 to w - 2 do
      try
	let xn ,yn ,zn  = HeightField.normal hf x z (succ x) (succ z) (succ x) z in
	Raw.sets_float n ((z * w + x + 1) * 3) [| xn; yn; zn |]
      with _ -> (Printf.fprintf stderr "normals : %d %d" x z; exit 1)
    done;
  done;
  let index x y = ((y * w) + x) in
  let e = Array.init (h - 2) (fun z -> 
    let e = Raw.create_static `uint count in
    Gc.finalise (fun x -> print_endline "freeing static raw..."; Raw.free_static x) e;
    for x = 0 to w - 2 do
      try 
	let x1  = index (    x) (     z)
	and x2  = index (    x) (succ z) in
	Raw.sets e (x * 2) [| x2; x1 |]
      with _ -> (Printf.fprintf stderr "elements : %d %d" x z; exit 1)
    done;
    e
  ) in 
  new hf_element_render hf r n e
    
(* ------------------------ *)

(*
(* adjacency not supported by lablgl! *)
class hf_adjacent_render hf r n e =
  let w,h = HeightField.dims hf in
object(self)
    
  method render_normals () = 
    let render_normal hf x z x' z' x'' z'' =
      let xn,yn,zn = HeightField.normal hf x z x' z' x'' z''
      and x,y,z = HeightField.center hf x z x' z' x'' z'' in
      Gl.disable `lighting;
      GlDraw.color (0.,0.,1.);
      GlDraw.begins `lines;
      GlDraw.vertex3 (x, y, z);
      GlDraw.vertex3 (x +. xn, y +. yn, z +. zn);
      GlDraw.ends ();
      GlDraw.point_size 2.0;
      GlDraw.begins `points;
      GlDraw.vertex3 (x +. xn, y +. yn, z +. zn);
      GlDraw.ends ();
      GlDraw.point_size 1.0
    in
    for z = 0 to h - 2 do
      for x = 0 to w - 2 do
	render_normal hf x z (succ x) (succ z) (succ x) z;
	render_normal hf x z x (succ z) (succ x) (succ z);
      done;
    done

  method refresh () =
    GlArray.vertex `three r;
    GlArray.normal n

  method render () : unit =
    GlArray.enable `vertex;
    GlArray.enable `normal;
    for i = 0 to (h - 2) do 
      GlArray.draw_elements `triangle_strip_adjacency ~count e.(i)
    done;
    GlArray.disable `vertex;
    GlArray.disable `normal

end

let hf_adjacent () =
  let w = 400
  and h = 400 in
  let hf = HeightField.mountains w h in
  let count = ((pred w) / 2) * 12 in
  let r = Raw.create_static `double (h * w * 3) in
  Gc.finalise Raw.free_static r;
  for z = 0 to h - 1 do
    for x = 0 to w - 1 do
      let vx,vy,vz = HeightField.get hf x z in
      Raw.sets_float r ((z * w + x) * 3) [| vx; vy; vz |];
    done;
  done;
  let ccount = (pred h) * w * 6 in
  let n = Raw.create_static `double ccount in
  Gc.finalise Raw.free_static n;
  for z = 0 to h - 2 do
    for x = 0 to w - 2 do
      let xn ,yn ,zn  = HeightField.normal hf x z (succ x) (succ z) (succ x) z
      and xn',yn',zn' = HeightField.normal hf x z x (succ z) (succ x) (succ z) in
      Raw.sets_float n ((z * w + x + 1) * 6) [| xn; yn; zn; xn'; yn'; zn' |]
    done;
  done;
  let index x y = ((y * w) + x) * 3 in
  let e = Array.init (h - 2) (fun i -> 
    let e = Raw.create_static `uint count in
    Gc.finalise Raw.free_static e;
    for x = 1 to (w - 1) / 2 do
      let x = x * 2 in
      let x1  = index (     x) (     z)
      and x2  = index (pred x) (succ z)
      and x3  = index (     x) (succ z)
      and x4  = index (succ x) (pred z)
      and x5  = index (succ x) (     z)
      and x6  = index (     x) ( 2 + z)
      and x7  = index (succ x) (succ z)
      and x8  = index ( 2 + x) (pred z)
      and x9  = index ( 2 + x) (     z)
      and x10 = index ( 2 + x) (     z)
      and x11 = index ( 2 + x) (succ z)
      and x12 = index ( 3 + x) (     z)
      in
      Raw.sets e (x * 12) [| x1; x2; x3; x4; x5; x6; x7; x8; x9; x10; x11; x12 |]
    done;
    e) in
  new hf_adjacency_render hf r n e

  *)

(* ---------------------------------------------------------------------- *)

class view screen = 
object(self)

  val grid = new grid

  method private draw_grid () =
    List.iter Gl.enable [
      `blend; 
      `alpha_test; 
    ];
    List.iter Gl.disable [
      `lighting;
      `depth_test;
      `texture_2d;
    ];
    GlDraw.polygon_mode `both `line;
    GlDraw.color (0.3,0.0,0.0);
    grid#render ();
    GlDraw.color (1.,1.,1.);
    List.iter Gl.enable [
      `lighting;
      `depth_test;
      `texture_2d;
    ];
    List.iter Gl.disable [
      `blend; 
      `alpha_test; 
    ]

  val hf = hf_elements 300 300

  val mutable normal_rendering = false

  method private draw_hf () =
    GlDraw.polygon_mode `front `fill;
    GlDraw.color (0.3,0.3,0.3);
    hf#render ();
    if normal_rendering then
      hf#render_normals ()

  val light = new light 0

  (* ---------------------------------- *)

  val mutable pos = Vector.of_tuple (0.,-5.,-15.,1.0)

  method position = pos

  method set_position v = pos <- v

  (* ---------------------------------- *)

  val mutable focale_ = 1.5

  method focale = focale_

  method set_focale v = focale_ <- v

  (* ---------------------------------- *)

  val mutable depth_ = 60.

  method add_depth a =
    depth_ <- depth_ +. a;
    self#reshape ()

  (* ---------------------------------- *)

  val mutable scale_ = 0.5

  method scale = scale_

  method set_scale v = scale_ <- v

  (* view orientation ----------------- *)

  val mutable vdir = Array.make 3 (Vector.null ())

  method update_vdir () =
    let mat = GlMat.to_array (GlMat.get_matrix `modelview_matrix)
    in
      vdir.(0) <- Vector.of_tuple (mat.(0).(0), mat.(1).(0), mat.(2).(0), 1.0);
      vdir.(1) <- Vector.of_tuple (mat.(0).(1), mat.(1).(1), mat.(2).(1), 1.0);
      vdir.(2) <- Vector.of_tuple (mat.(0).(2), mat.(1).(2), mat.(2).(2), 1.0)

  method front = vdir.(2)
  method up    = vdir.(1)
  method right = vdir.(0)

  val mutable view_rot = [| 0.0 ; 0.0 ; 0.0 |]

  val modangle = fun x -> if x > 180. then (x -. 360.) else if (x < -180.) then (x +. 360.) else x

  method rotx  a = view_rot.(0) <- modangle (view_rot.(0) +. a)
  method roty  a = view_rot.(1) <- modangle (view_rot.(1) +. a)
  method rotz  a = view_rot.(2) <- modangle (view_rot.(2) +. a)

  (* ---------------------------------- *)

  (* set the screen geometry *)
  method setviewport x y width height =
    GlDraw.viewport ~x:x ~y:x ~w:width ~h:height


  (* view toolbox ------ *)
  method view_setup () =
    GlMat.mode `modelview;
    GlMat.load_identity(); 
(*
    (* swap y and z *)
    GlMat.rotate ~angle:90.0 ~x:(1.0) ();
    GlMat.scale  ~y:(-.1.0) ();
*)
    GlMat.rotate ~angle:view_rot.(0) ~x:1.0 ();
    GlMat.rotate ~angle:view_rot.(1) ~y:1.0 ();
    GlMat.rotate ~angle:view_rot.(2) ~z:1.0 ();
    GlMat.translate3 (pos.(0),pos.(1),pos.(2));
    self#update_vdir ();
    GlMat.scale3 (scale_,scale_,scale_);

  (* clear the display buffers *)
  method clear_buffer () =
    GlClear.clear [`color;`depth]

  method draw_setup () =
    self#clear_buffer ();
    self#view_setup ()

  (* draw the whole thing *)
  method draw () =
    self#draw_setup ();
    self#draw_grid ();
    light#render ();
    self#draw_hf ()

  method display () : unit =
    self#draw ();
    screen#finish ()

  method set_frustum min max =
    GlMat.mode `projection;
    GlMat.load_identity ();
    GlMat.frustum 
      ~x:( min.(0), max.(0) ) 
      ~y:( min.(1), max.(1) ) 
      ~z:( min.(2), max.(2) );
    GlMat.mode `modelview

  method reshape () =
    let width, height = screen#dims () in
    self#setviewport 0 0 width height;
    let r = float width /. float height
    and f = focale_ in
    if width > height
    then
      self#set_frustum [| -.r; -.1.0; f |] [| r; 1.0; depth_ |]
    else
      let r' = 1. /. r in
      self#set_frustum [| -1.;  -.r'; f |] [| 1.; r'; depth_ |]

end

(* ---------------------------------------------------------------------- *)

class sdl_controller view =
object(self)

  val mutable active_ = true

  method active = active_

  (* ---------------------------------- *)

  val mutable old_xmcoord = 0
  val mutable old_ymcoord = 0

  (* moving bitfield *)
  val move_rotate    = 0x01
  val move_z         = 0x02
  val move_x         = 0x04
  val move_y         = 0x08
  val mutable moving = 0

  (* moving speed *)
  val mutable speed       = [| 0. ;  0. ; 0. |]
  val mutable speed_fact  = 0.1

  (* ---------------------------------- *)

  val mutable motion_handler = fun a b -> ()

  method motion_rot ~xmcoord ~ymcoord =
    view#rotx ((float ymcoord) -. (float old_ymcoord)); old_ymcoord <- ymcoord;
    view#roty ((float xmcoord) -. (float old_xmcoord)); old_xmcoord <- xmcoord

  method motion_scale ~xmcoord =
    let limit = 0.01 in
    let x = view#scale +. (float (xmcoord - old_xmcoord)) /. 100.0 in 
    view#set_scale (max x limit);
    old_xmcoord <- xmcoord

  method motion_focale ~xmcoord =
    let limit = 1.0 in
    let x = view#focale +. (float (xmcoord - old_xmcoord)) /. 1000.0 in 
    view#set_focale (max x limit);
    view#reshape ();
    old_xmcoord <- xmcoord

  method motion_off = ()

  method mouse ~btn ~state ~xmcoord ~ymcoord =
    match state with 
	RELEASED -> (
	  moving <- moving land (lnot move_rotate);
	  match btn with
	      BUTTON_LEFT  -> 
		self#set_motion (fun a b  -> self#motion_off );
	    | _            ->
		self#set_motion (fun a b  -> self#motion_off )
	)
      | PRESSED  -> (
	  moving <- moving lor move_rotate;
	  match btn with
	      BUTTON_LEFT  -> (
		old_xmcoord <- xmcoord;
		old_ymcoord <- ymcoord;
		self#set_motion (fun a b  -> self#motion_rot a b)
	      )
	    | BUTTON_RIGHT -> (
		old_xmcoord <- xmcoord;
		self#set_motion (fun a b  -> self#motion_scale a )
	      )
	    | BUTTON_MIDDLE -> (
		old_xmcoord <- xmcoord;
		self#set_motion (fun a b  -> self#motion_focale a )
	      )
	    | BUTTON_WHEELUP   -> view#add_depth 10.
	    | BUTTON_WHEELDOWN -> view#add_depth (-10.)
 	)

  method keyboard ~key ~state =
    let move k fact flag =
      moving <- moving lor flag;
      speed.(k) <- speed.(k) +. (float fact)
    and stop k fact flag =
      moving <- moving land (lnot flag);
      speed.(k) <- speed.(k) -. (float fact)
    in
    match state with 
	RELEASED -> (
	  match key with
	    | KEY_RIGHT    | KEY_f       -> stop 0 ~-1 move_x
	    | KEY_LEFT     | KEY_s       -> stop 0   1 move_x
	    | KEY_UP       | KEY_e       -> stop 2   1 move_y
	    | KEY_DOWN     | KEY_d       -> stop 2 ~-1 move_y
	    | KEY_PAGEUP   | KEY_t       -> stop 1 ~-1 move_z
	    | KEY_PAGEDOWN | KEY_g       -> stop 1   1 move_z
	    | KEY_LSHIFT   | KEY_RSHIFT  -> speed_fact <- 0.1
	    | _           -> ()
	)
      | PRESSED ->
	  match key with
	      KEY_UNKNOWN -> ()
	    | KEY_ESCAPE  -> active_ <- false
	    | KEY_r       -> view#reshape ();
	    | KEY_RIGHT    | KEY_f       -> move 0 ~-1 move_x
	    | KEY_LEFT     | KEY_s       -> move 0   1 move_x
	    | KEY_UP       | KEY_e       -> move 2   1 move_y
	    | KEY_DOWN     | KEY_d       -> move 2 ~-1 move_y
	    | KEY_PAGEUP   | KEY_t       -> move 1 ~-1 move_z
	    | KEY_PAGEDOWN | KEY_g       -> move 1   1 move_z
	    | KEY_LSHIFT   | KEY_RSHIFT  -> speed_fact <- 1.0
	    | _           -> ()

  method motion x y = motion_handler x y 

  (* SDL specific ---------------------------------------------- *)

  method event = function
    | KEYDOWN           k 
    | KEYUP             k        -> self#keyboard ~state:k.ke_state ~key:k.keysym
    | MOUSEBUTTONDOWN   m
    | MOUSEBUTTONUP     m        -> self#mouse ~btn:m.mbe_button ~state:m.mbe_state ~xmcoord:m.mbe_x ~ymcoord:m.mbe_y
    | MOUSEMOTION       m        -> self#motion m.mme_x m.mme_y
    | VIDEORESIZE (width,height) -> (
      view#reshape ()
    )
    | _                          -> (
      prerr_endline "unhandled event";
      flush stderr
    )

  val mutable timing = 0.
  val mutable frames = 0

  method act () =
    let timeframe = 0.01 in
    let usleep t = ignore (Unix.select [] [] [] t ) in
    let t = Unix.gettimeofday () in
    view#display ();
    let pos = Vector.add view#position (Vector.scale view#right (speed.(0) *. speed_fact)) in
    let pos = Vector.add pos (Vector.scale view#up (speed.(1) *. speed_fact)) in
    let pos = Vector.add pos (Vector.scale view#front (speed.(2) *. speed_fact)) in
    view#set_position pos;
    let timelaps = (Unix.gettimeofday ()) -. t in
    timing <- timing +. timelaps;
    frames <- succ frames;
    if timing > 1.0
    then (
      timing <- timing -. 1.;
      prerr_endline ("fps: "^string_of_int frames);
      frames <- 0;
    );
    let t = timeframe -. timelaps in
    if t > 0.0 then usleep t else ()

  method set_motion f = motion_handler <- f

  initializer (
    self#set_motion (fun a b -> self#motion_off)
  )

end

(* ---------------------------------------------------------------------- *)

class sdl_screen surface =
object(self)

  val mutable surface_ : Sdlvideo.surface = surface

  method dims () =
    let x,y,_ = Sdlvideo.surface_dims surface_ in
    x,y

  method width () =
    fst(self#dims ())

  method height () = 
    snd(self#dims ())

  method bpp () =
    Sdlvideo.surface_bpp surface_

  method resize width height =
    let bpp = self#bpp () in
    surface_ <- set_video_mode ~w:width ~h:height ~bpp [`HWSURFACE ; `OPENGL]

  method private print_attr a = 
    let s, v = 
      match a with 
	  RED_SIZE i         -> "RED_SIZE", string_of_int i
	| GREEN_SIZE i       -> "GREEN_SIZE", string_of_int i 
	| BLUE_SIZE i        -> "BLUE_SIZE", string_of_int i
	| ALPHA_SIZE i       -> "ALPHA_SIZE", string_of_int i
	| BUFFER_SIZE i      -> "BUFFER_SIZE", string_of_int i
	| DOUBLEBUFFER b     -> "DOUBLEBUFFER", string_of_bool b
	| DEPTH_SIZE i       -> "DEPTH_SIZE", string_of_int i
	| STENCIL_SIZE i     -> "STENCIL_SIZE", string_of_int i
	| ACCUM_RED_SIZE i   -> "ACCUM_RED_SIZE", string_of_int i
	| ACCUM_GREEN_SIZE i -> "ACCUM_GREEN_SIZE", string_of_int i
	| ACCUM_BLUE_SIZE i  -> "ACCUM_BLUE_SIZE", string_of_int i
	| ACCUM_ALPHA_SIZE i -> "ACCUM_ALPHA_SIZE", string_of_int i
	| STEREO i           -> "STEREO", string_of_int i
    in
    print_string s; print_string " "; print_string v; print_newline ()

  method dump_attr () =
    List.iter self#print_attr (Sdlgl.get_attr ());

  method finish () =
    Sdlgl.swap_buffers ()

  method shutdown () =
    Sdl.quit ()

  method private init () =
    GlDraw.cull_face `back;
    GlDraw.front_face `cw;
    GlTex.env `texture_env (`mode `decal);
    List.iter Gl.enable [
      `cull_face;
      `lighting;
      `depth_test;
    ];
    GlDraw.shade_model `smooth;
    Sdlgl.set_attr [
      RED_SIZE 5;
      GREEN_SIZE 5;
      BLUE_SIZE 5;
      DEPTH_SIZE 16;
      DOUBLEBUFFER true
    ]

  initializer (
    self#init ()
  )

end

let sdl_screen ~(title:string) ~(width:int) ~(height:int) =
  Sdl.init [`EVERYTHING];
  let screen = set_video_mode ~w:width ~h:height ~bpp:32 [`HWSURFACE ; `OPENGL] in
  Sdlwm.set_caption title title;
  new sdl_screen screen

(* ---------------------------------------------------------------------- *)

class app =
  let screen = sdl_screen ~title:"perlin noise based terrain generator" ~width:800 ~height:600 in
  let view = new view screen in
  let ctrl = new sdl_controller view in
object(self)

  method private init () =
    view#reshape ()

  method poll () =
    Sdlevent.poll ()

  method check_events () =
    let rec check_events () =
    match self#poll () with
      | None   -> ()
      | Some e -> (
	match e with 
	  | VIDEORESIZE (width,height) -> (
	    screen#resize width height;
	    view#reshape ()
	  )
	  | _                          -> (
	    ctrl#event e
	  )
      ); check_events ()
    in
    check_events ()

  method loop () =
    while ctrl#active do
      self#check_events ();
      ctrl#act ()
    done;
    screen#shutdown ()

  initializer(
    self#init ()
  )

end

let _ =
  begin 
    (new app)#loop ();
    if not !Sys.interactive then begin
      exit 0;
    end;
  end
