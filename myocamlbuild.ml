open Ocamlbuild_plugin;;
open Command;;

dispatch begin function
  | After_rules ->

    let lablgl  = "+lablGL"
    and sdl     = "+sdl"
    and glmlite = "+glMLite"
    in

    (* add ulex preprocessor for use_ulex tag 
    flag ["ocaml";"pp";"use_ulex"] (S[A "camlp4o";A "-I";A "/usr/lib/ocaml/ulex";A "pa_ulex.cma"]);
    flag ["ocaml";"use_ulex"] (S[A "-I";A "/usr/lib/ocaml/ulex"]);
    *)

    flag ["ocaml";"compile";"native";"inline"] (S [A "-inline"; A "26"]);
    flag ["ocaml";"compile";"native";"unsafe"] (S [A "-unsafe"]);
    flag ["ocaml";"compile";"native";"asm"] (S [A "-S"]);

    flag ["use_glMLite";"compile"] (S [A "-I"; A glmlite]);

    List.iter (fun (dir,nl) -> List.iter (fun name -> ocaml_lib ~extern:true ~dir name) nl) [
      (lablgl, [
	 "lablgl";
	 "lablglut";
	 "gl";
	 "glArray";
	 "glClear";
	 "glDraw";
	 "glFunc";
	 "glLight";
	 "glList";
	 "glMap";
	 "glMat";
	 "glMisc";
	 "glPix";
	 "glTex";
	 "gluMat";
	 "gluMisc";
	 "gluNurbs";
	 "gluQuadric";
	 "glut";
	 "gluTess";
	 "raw";
	 "togl"
       ]
      );

      (glmlite, [
	 "GL";
	 "FunGL";
	 "FunGlut";
	 "Glu";
	 "Glut";
	 "jpeg_loader";
	 "png_loader";
	 "svg_loader";
	 "VBO";
	 "vertArray"
       ]);

      (sdl, [
	 "sdl";
	 "sdlcdrom";
	 (* "sdlevent"; *)
	 "sdlgl";
	 "sdljoystick";
	 "sdlkey";
	 "sdlloader"; 
	 "sdlmixer";
	 "sdlmouse";
	 "sdltimer";
	 "sdlttf";
	 "sdlvideo"; 
	 "sdlwm"
       ]
      ); 
    ];
  | _ -> ()
end;;
