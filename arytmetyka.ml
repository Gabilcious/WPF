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

let dz x y = 
	match x, y with
	|x, y -> x/.y


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

let plus x y =
	let rec dodaj x y z wyn =
		match x, y with
		| _, [] -> wyn
		| [], _::t -> dodaj z t z wyn
		| (ax,bx)::tx, (a,b)::_ -> dodaj tx y z (przedzial (ax+.a) (bx+.b) wyn)
	in match y with
	| [] -> x
	| _ -> dodaj x y x []


let minus x y =
	let rec odejmij x y z wyn =
		match x, y with
		| _, [] -> wyn
		| [], _::t -> odejmij z t z wyn
		| (ax, bx)::tx, (a,b)::_ -> odejmij tx y z (przedzial (ax-.b) (bx-.a) wyn)
	in match y with
	| [] -> x
	| _ -> odejmij x y x []


let razy x y =
	let rec mnoz x y z wyn =
		match x, y with
		| _, [] -> wyn
		| [], _::t -> mnoz z t z wyn
		| (ax,bx)::tx, (a,b)::_ -> mnoz tx y z (przedzial (minimum (mn ax a) (mn ax b) (mn bx a) (mn bx b)) (maximum (mn ax a) (mn ax b) (mn bx a) (mn bx b)) wyn)
	in match y with
	| [] -> x
	| _ -> mnoz x y x []


let podzielic x y =
	let rec dziel x y z wyn =
		match x, y with
		| _, [] -> wyn
		| [], _::t -> dziel z t z wyn
		| (ax,bx)::tx, (a,b)::_ -> dziel tx y z (przedzial (minimum (dz ax a) (dz ax b) (dz bx a) (dz bx b)) (maximum (dz ax a) (dz ax b) (dz bx a) (dz bx b)) wyn)
	in match y with
	| [] -> x
	| _ -> dziel x y x []
