type t =
    | Empty
    | Node of t * (int * int) * t * (int * int)
(* lewe poddrzewo * przedział w wierzchołku * prawe poddrzewo * wysokość i ilosc elementów *)

(***********************
       POMOCNICZE       
***********************)

let cmp (a, b) (c, d) =
    if b < c then -1 else
    if a > d then 1 else 0
(* własne porównywanie par: zwraca 0, gdy przedziały nachodzą na siebie *)

let height = function
    | Node (_, _, _, (h,_) ) -> h
    | Empty -> 0

let count = function
    | Node (_, _, _, (_,c) ) -> c
    | Empty -> 0

let add_num a b =
    let (a, b) = (min a b), (max a b) in
    if a >= 0 then
        if a >= max_int - b then max_int
        else a + b
    else if b <= (min_int - a) then min_int else a + b
(* dodawanie, aby nigdy nie przekroczyć wartości min_int i max_int *)

let add_num2 a b czy_min_int =
    if czy_min_int then
        if a >= -2 then max_int
        else -(min_int - a )
    else add_num a b
(* osobna funckja na dodawanie uwzględniająca, że min_int != max_int *)

let make l ((a, b) as k) r = Node (l, k, r, (max (height l) (height r) + 1,
    add_num (add_num (count l) (count r)) (add_num (add_num2 b (-a) (a = min_int))  1)) )

let bal l k r =
    let hl = height l in
    let hr = height r in
    if hl > hr + 2 then
        match l with
        | Node (ll, lk, lr, _) ->
            if height ll >= height lr then make ll lk (make lr k r)
            else (match lr with
                | Node (lrl, lrk, lrr, _) ->
                    make (make ll lk lrl) lrk (make lrr k r)
                | Empty -> assert false)
        | Empty -> assert false
    else if hr > hl + 2 then
        match r with
        | Node (rl, rk, rr, _) ->
            if height rr >= height rl then make (make l k rl) rk rr
            else (match rl with
                | Node (rll, rlk, rlr, _) ->
                    make (make l k rll) rlk (make rlr rk rr)
                | Empty -> assert false)
        | Empty -> assert false
    else make l k r

let rec min_elt = function
    | Node (Empty, k, _, _) -> k
    | Node (l, _, _, _) -> min_elt l
    | Empty -> raise Not_found

let rec remove_min_elt = function
    | Node (Empty, (_,b), r, _) -> b, r
    | Node (l, k, r, _) -> let (x, set) = remove_min_elt l in x, bal set k r
    | Empty -> invalid_arg "PSet.remove_min_elt"

let rec remove_max_elt = function
    | Node (l, (a,_), Empty, _) -> a, l
    | Node (l, k, r, _) -> let (x, set) = remove_max_elt r in x, bal set k l
    | Empty -> invalid_arg "PSet.remove_max_elt"

let merge t1 t2 =
    match t1, t2 with
    | Empty, _ -> t2
    | _, Empty -> t1
    | _ -> let k = min_elt t2 in
           bal t1 k (let (_,set) = remove_min_elt t2 in set)

let rec join l v r =
    match (l, r) with
        | (Empty, _) -> add v r
        | (_, Empty) -> add v l
        | (Node(ll, lv, lr, (lh,_)), Node(rl, rv, rr, (rh,_))) ->
            if lh > rh + 2 then bal ll lv (join lr v r) else
            if rh > lh + 2 then bal (join l v rl) rv rr else
                make l v r

and split x set =
    let rec loop x = function
        | Empty -> (Empty, false, Empty)
        | Node (l, ((va, vb) as v), r, _) ->
            let c = cmp (x,x) v in
            if c = 0 then
                ((if x > va then add (va, x - 1) l else l),
                true,
                (if x < vb then add (x + 1, vb) r else r))
            else if c < 0 then
                let (ll, pres, rl) = loop x l in (ll, pres, join rl v r)
            else
                let (lr, pres, rr) = loop x r in (join l v lr, pres, rr) in
    loop x set

and add (a, b) set =
    let (l, presl, _) = split (a - 1) set in
    let (_, presr, r) = split (b + 1) set in
    let (a, l) = if presl then remove_max_elt l else a, l in
    let (b, r) = if presr then remove_min_elt r else b, r in
    join l (a, b) r

(***********************
      KONSTRUKTORY      
***********************)

let empty = Empty

(***********************
        SELEKTORY       
***********************)

let is_empty set = set = Empty

let mem x set =
    let rec loop = function
        | Node (l, k, r, _) ->
            let c = cmp (x, x) k in
            c = 0 || loop (if c < 0 then l else r)
        | Empty -> false in
    loop set

let exists = mem

let elements set =
    let rec loop acc = function
        | Empty -> acc
        | Node (l,k,r,_) -> loop (k :: loop acc r) l in
    loop [] set  

let below x set =
    let (l,pres,_) = split x set in
    add_num (count l) (if pres then 1 else 0)

(***********************
      MODYFIKATORY      
***********************)

let remove (a,b) set =
    let (l, _, _) = split a set in
    let (_, _, r) = split b set in
    merge l r
    
let iter f set =
    let rec loop = function
        | Empty -> ()
        | Node (l,k,r,_) -> loop l; f k; loop r in
    loop set

let fold f set acc =
    let rec loop acc = function
        | Empty -> acc
        | Node(l,k,r,_) -> loop (f k (loop acc l)) r in
    loop acc set
