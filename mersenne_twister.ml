(* ML translation of the following:
   A C-program for MT19937: Integer version (1998/4/6)            
   genrand() generates one pseudorandom unsigned integer (32bit) 
   which is uniformly distributed among 0 to 2^32-1  for each     
   call. sgenrand(seed) set initial values to the working area    
   of 624 words. Before genrand(), sgenrand(seed) must be         
   called once. (seed is any 32-bit integer except for 0).        
   Coded by Takuji Nishimura, considering the suggestions by    
   Topher Cooper and Marc Rieffel in July-Aug. 1997.              

   This library is free software; you can redistribute it and/or   
   modify it under the terms of the GNU Library General Public     
   License as published by the Free Software Foundation; either    
   version 2 of the License, or (at your option) any later         
   version.                                                        
   This library is distributed in the hope that it will be useful, 
   but WITHOUT ANY WARRANTY; without even the implied warranty of  
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.            
   See the GNU Library General Public License for more details.    
   You should have received a copy of the GNU Library General      
   Public License along with this library; if not, write to the    
   Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA    
   02111-1307  USA                                                 

   Copyright (C) 1997 Makoto Matsumoto and Takuji Nishimura.       
   When you use this, send an email to: matumoto@math.keio.ac.jp   
   with an appropriate reference to your work.                     

   REFERENCE                                                       
   M. Matsumoto and T. Nishimura,                                  
   "Mersenne Twister: A 623-Dimensionally Equidistributed Uniform  
   Pseudo-Random Number Generator",                                
   ACM Transactions on Modeling and Computer Simulation,           
   Vol. 8, No. 1, January 1998, pp 3--30.                          *)

(* Period parameters *)
module Period =
struct 
  let n = 624
  let m = 397
  let matrix_a   = 0x9908b0df (* constant vector a *)
  let upper_mask = 0x80000000 (* most significant w-r bits *)
  let lower_mask = 0x7fffffff (* least significant r bits *)
end

(* tempering parameters *)
module Tempering =
struct
  let mask_b = 0x9d2c5680
  let mask_c = 0xefc60000
  let shift_u y = (y >> 11)
  let shift_s y = (y << 7)
  let shift_t y = (y << 15)
  let shift_l y = (y >> 18)
end

module State =
struct
  type t = {mt : int Array; mutable idx : int}

  let create () = {mt = Array.make Period.n 0 (* the array for the state vector  *); idx = 0 }

  (* initializing the array with a NONZERO seed *)
  let init v seed =
    (* setting initial seeds to mt[N] using         
       the generator Line 25 of Table 1 in          
       [KNUTH 1981, The Art of Computer Programming 
       Vol. 2 (2nd Ed.), pp102]                     *)
    v.mt.(0) <- seed land 0xffffffff ;
    for i = 1 to (pred N) do
      v.mt.(i) <- (69069 * v.mt.(i-1)) land 0xffffffff
    done;
    v
      
end

let default = State.init (State.create ()) 4357

let rand () = 
  let mag01 = [| 0x0; Period.matrix_a |]
    (* mag01[x] = x * MATRIX_A  for x=0,1 *)
  in
  let refill s =
    let newy s v1 v2 = 
      (s.mt.(v1) land Period.upper_mask) lor (s.mt.(v2) land Period.lower_mask)
    and storemt s idx1 idx2 y =
      s.mt.(idx1) <- (s.mt.(idx2) lxor (y lsr 1)) lxor mag01.(y land 0x1)
    in
      for kk = 0 to Period.n - Period.m - 1 do 
	let y = newy s kk (kk+1)
	in
          storemt s kk (kk+Period.m) y 
      done;
      for kk = Period.n - Period.m to Period.n - 2 do
	let y = newy s kk (kk+1)
	in
	  storemt s kk (kk+(Period.m)) y 
      done;
      let y = newy s (Period.n - 1) 0
      in
	storemt s (Period.n - 1) (Period.m - 1) y 
  in
  let s = default in
    if s.idx >= Period.n 
    then refill s;
    let y = s.mt.(s.idx) in
    let y = (y lxor (Tempering.shift_u y)) in
    let y = (y lxor ((Tempering.shift_s y) land Tempering.mask_b)) in
    let y = (y lxor ((Tempering.shift_t y) land Tempering.mask_c)) in
      y lxor (Tempering.shift_l y)
