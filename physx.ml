open Graphics ;;
#load "graphics.cma" ;;
#load "unix.cma" ;;

let fps = 15. ;;

type vect = float * float * float * float ;;

let ti = int_of_float and tf = float_of_int ;;

let gravity = (0. ,0. ,0. ,-9.81 /. 4. ) ;;
let pi = 3.1415 ;;

let collision i p = match i,p with
	|(x,y,_,_) , (a,b,c,d) ->
	(x >= a && x <= (a +. c))
	&& (y >= b && y <= (b +. d)) ;; 
	
module Vector = struct

let create (v:float*float*float*float) : vect = v ;;
	
let add (v1: vect) (v2: vect) : vect = 
	match v1 , v2 with
	| (x,y,w,h) , (a,b,c,d) ->
	(x , y , (w +. c) , (h +. d) ) ;;
	
let multi (v1: vect ) (v2: vect) : vect = 
	match v1 , v2 with
	| (x,y,w,h) , (a,b,c,d) ->
		( x , y , ( w *. c ) , ( h *. d ) ) ;;
	
let  scalar (v: vect ) (k: float) : vect = 
	match v with
	| (x,y,w,h) ->
	 ( x , y , ( w *. k ) , ( h *. k ) ) ;;

let module2d (v: vect) = 
	match v with
	| (x,y,w,h) ->
	let s = (w -. x) ** 2. +. (h -. y) ** 2. in
	sqrt s

let rotate2d (v: vect) (teta: float) : vect =
	match v with | (x,y,w,h) -> 
	let nw = (w -. x) and nh = (h -. y) in
	(x,y,
	( nw *. (cos teta) -. nh *. (sin teta) ),
	( nh *. (cos teta) +. nw *. (sin teta) ) )
	

end;;

module Time = struct

let sec = Unix.time() ;;
let msec = Unix.gettimeofday() *. 1000. ;;

let wait (ms : float) : unit =
	let r = msec in
	let t = ref r and t_old = ref r in
		while !t < (!t_old +. (10000. /. ms) ) do 
			t := !t +. 0.001 ;
			done ;;
end;;

class body = object

val mutable dim = (0. , 0. ,0. ,0.)

val mutable v = (0. ,0. ,0. ,0.)

method masse = let (_,_,h,w) = dim in ( w *. h) /. 100.

method get_vct = v
method get_dim = match v,dim with 
|(x,y,_,_) ,(_,_,e,r)-> (x,y,e,r)
	
method set_vct p = match p with |(x,y,w,h) ->
	v <- (x, y, (x +. w) ,(y +.h) )
method set_dim u = dim <- u 

(* function  important togo to the new y x *)
method apply (l : vect list) : vect =
	let rec  aux l = match l with
	| [] -> (0. ,0. ,0. ,0.)
	| e::fin -> Vector.add v ( aux fin ) in aux l 

method draw c vc = match v,dim with
		| (x,y,w,h),(_,_,e,r) ->
		
		set_color c ;
		fill_rect (ti x) (ti y) (ti e) (ti r);
		set_color vc;
		draw_segments 
		[|( (ti (x +. (e /. 2.))),
		(ti (y +. (r /. 2.))),(ti w),(ti h) )|];
end;;

let setScreen =
  open_graph "";
  resize_window 400 400;
  set_window_title "Physics" ;;
  
let exec = 
	let s = wait_next_event [Button_down; Key_pressed] 
     in if s.Graphics.keypressed then
      false else true ;;

let detect (f:body) (s: body) : vect  =
let (x,y,w,h) = f#get_vct in

if (collision f#get_dim s#get_dim) 
|| (collision s#get_dim f#get_dim) = true then

begin
(* inverse vecotr *)
let nw = (w -. x) and nh = (h -. y) in
if (nw  >= 0. || nw  < 0.) && nh < 0.
then ( x,y, nw ,(-.nh) ) else ( x,y,(-.nw) , nh  ) 
end 

else Vector.add (x,y,(w -. x) ,(h -. y)) gravity ;;


let translate (v: vect) (i: float) =
match v with | (x ,y ,w ,h) ->
	
	let hyp = Vector.module2d v
	and adj = sqrt ((w -. x) ** 2.) in
 	let teta = (acos (adj /. hyp) ) in 

	let nadj = max ((cos teta ) *. (min hyp i) )  0.
	and nopp = max ((sin teta ) *. (min hyp i) )  0.
	in

	let px = if x > w then -1. *. nadj else nadj
	and py = if y > h then -1. *. nopp else nopp in
(
(x +. px),( y +. py),
((w -. x) -. px) , ((h -. y) -. py)
) ;;


let main =

let sol = new body in 
(sol#set_vct(0. ,0. ,0. ,0.));
(sol#set_dim(0.,0.,400. ,40.));

let box = new body in 
(box#set_vct(50. ,80. , 180. , -40.));
(box#set_dim(50.,80.,15. ,15.));

while exec do
	
	set_color white ;
	fill_rect 0 0 400 400 ;
	
	let t = detect box sol in box#set_vct t ;
	box#set_vct (translate box#get_vct 1. ) ;

	box#draw red blue ;
	sol#draw black black ;
	Time.wait fps;
done;;

(** Exec **)

setScreen ;;
main exec ;;
