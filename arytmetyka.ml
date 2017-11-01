type wartosc = (float * float) list

(***********************
       POMOCNICZE       
***********************)

let toNan a = not (a <= infinity && a >= neg_infinity)

let rec przedzial x y wyn =
	if toNan x || toNan y then (x,y)::wyn
	else if x > y then przedzial y x wyn
	else if y > 0. && x = -0. then (0.,y)::wyn
	else if x < 0. && y = 0. then (x,-0.)::wyn
	else if x*.y >= 0. then (x,y)::wyn
	else (x,-0.)::((0.,y)::wyn)

let min a b = if toNan a then b else if toNan b then a else if a < b then a else b;;
let max a b = if toNan a then b else if toNan b then a else if a > b then a else b;;

let minimum a b c d = min (min a b) (min c d)
let maximum a b c d = max (max a b) (max c d)

let mn x y = 
	match x, y with
	|x, y when ((x = infinity) || x = neg_infinity) && (y = 0. || y = -0.) -> 0.
	|y, x when ((x = infinity) || x = neg_infinity) && (y = 0. || y = -0.) -> 0.
	|x, y -> x*.y

let dz x y = if y = 0 then nan else x /. y 

(***********************
      KONSTRUKTORY      
***********************)

let wartosc_dokladnosc x p = przedzial (x-.(p/.100.*.x)) (x+.(p/.100.*.x)) []
let wartosc_od_do x y = przedzial x y []
let wartosc_dokladna x = przedzial x x []

(***********************
        SELEKTORY       
***********************)

let rec in_wartosc x y =
	match x with
		| [] -> false
		| (a,b)::t -> if y >= a && y <= b
				then true
				else in_wartosc t y

let max_wartosc x =
	let rec maks x wyn =
		match x with
			| [] -> wyn
			| (_,b)::t -> maks t (max b wyn)
	in match x with
		| [] -> infinity
		| _ -> maks x neg_infinity

let min_wartosc x =
	let rec mini x wyn =
		match x with
			| [] -> wyn
			| (a,_)::t -> mini t (min wyn a)
	in match x with
		| [] -> neg_infinity
		| _ -> mini x infinity

let sr_wartosc x = ((min_wartosc x)+.(max_wartosc x))/.2.

(***********************
         OPERACJE       
***********************)

let operacja x y opr =
	let rec operuj x y z wyn =
		match x, y with
		| _, [] -> wyn
		| [], _::t -> operuj z t z wyn
		| (ax,bx)::tx, (a,b)::_ -> operuj tx y z (przedzial (opr ax a) (opr bx b) wyn)
	in match y with
	| [] -> x
	| _ -> operuj x y x []

let plus x y = operacja x y ( +. )
let minus x y = operacja x y ( -. )
let razy x y = operacja x y mn
let podzielic x y = operacja x y dz
