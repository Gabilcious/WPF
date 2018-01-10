open List

type point = float * float

type kartka = point -> int

(***********************
       POMOCNICZE       
***********************)

let epsilon = 0.000000001

let skalar a b c =
    (fst b -. fst a) *. (fst c -. fst a) +. (snd b -. snd a) *. (snd c -. snd a)

let odbicie (a1, b1) (a2, b2) (x, y) = 
    let pom = skalar (a1, b1) (a2, b2) (x, y) /. skalar (a1, b1) (a2, b2) (a2, b2) in
    (a1 +. pom *. (a2 -. a1), b1 +. pom *. (b2 -. b1))

let symetria p q (x, y) =
    let x2, y2 = odbicie p q (x, y) in
    (2. *. x2 -. x, 2. *. y2 -. y)

(***********************
      KONSTRUKTORY      
***********************)

let prostokat (a, b) (c, d) (x, y) =
    match (a <= x && x <= c && b <= y && y <= d) with
    | true -> 1
    | false -> 0

let kolko (a, b) r (x, y) =
    match ( (x -. a) *. (x -. a) +. (y -. b) *. (y -. b) <= r *. r) with
    | true -> 1
    | false -> 0

(***********************
      MODYFIKATORY      
***********************)

let zloz (a1, b1) (a2, b2) k (x, y)  = 
    let pom = (x -. a1) *. (b2 -. b1) -. (y -. b1) *. (a2 -. a1) in
    if pom > -1. *. epsilon && pom < epsilon then k (x, y)
    else if pom > 0. then 0
    else let q2 = symetria (a1, b1) (a2, b2) (x, y) in
           k (x, y) + k q2   

let skladaj lst origami =
    List.fold_left (fun acc (x, y) -> zloz x y acc) origami lst
