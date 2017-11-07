type wartosc = (float * float) list

let toNan a = not (a <= infinity && a >= neg_infinity)
let min a b = if toNan a then b else if toNan b then a else if a < b then a else b;;
let max a b = if toNan a then b else if toNan b then a else if a > b then a else b;;
let comp f a b c d = f (f a b) (f c d)

let rec przedzial x y wyn =
	if toNan x || toNan y then (x,y)::wyn
	else if x > y then przedzial y x wyn
	else if y > 0. && x = -0. then (0.,y)::wyn
	else if x < 0. && y = 0. then (x,-0.)::wyn
	else if x*.y >= 0. then (x,y)::wyn
	else (x,-0.)::((0.,y)::wyn)

let wartosc_dokladnosc x p = przedzial (x-.(p/.100.*.x)) (x+.(p/.100.*.x)) []
let wartosc_od_do x y = przedzial x y []
let wartosc_dokladna x = przedzial x x []

let rec in_wartosc x y =
	match x with
	| [] -> false
	| (a,b)::t -> if y >= a && y <= b then true else in_wartosc t y

let rec some_wartosc f x wyn = match x with
				| [] -> wyn
				| (a,b)::t -> some_wartosc f t (f a (f b wyn))

let max_wartosc x = some_wartosc max x nan
let min_wartosc x = some_wartosc min x nan
let sr_wartosc x = ((min_wartosc x)+.(max_wartosc x))/.2.

let operuj op x y =
	let rec oper x y z wyn =
		match x, y with
		| _, [] -> wyn
		| [], _::t -> oper z t z wyn
		| (ax,bx)::tx, (a,b)::_ -> oper tx y z (przedzial (comp min (op ax a) (op ax b) (op bx a) (op bx b)) (comp min (op ax a) (op ax b) (op bx a) (op bx b)) wyn)
	in match y with
	| [] -> x
	| _ -> oper x y x []

let plus x y = operuj ( +. ) x y
let minus x y = operuj ( -. ) x y
let razy x y = operuj ( *. ) x y
let podzielic x y = operuj ( /. ) x y
