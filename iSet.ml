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
(* osobna funckja na dodawanie uwzględniająca min_int != max_int *)

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
    | Node (Empty, _, r, _) -> r
    | Node (l, k, r, _) -> bal (remove_min_elt l) k r
    | Empty -> invalid_arg "PSet.remove_min_elt"

let merge t1 t2 =
    match t1, t2 with
    | Empty, _ -> t2
    | _, Empty -> t1
    | _ -> let k = min_elt t2 in
           bal t1 k (remove_min_elt t2)

let rec add_one x = function
    | Node (l, k, r, _) ->
        let c = cmp x k in
        if c < 0 then
            let nl = add_one x l in
            bal nl k r
        else
            let nr = add_one x r in
            bal l k nr
    | Empty -> make Empty x Empty
(* nigdy nie będzie c = 0, bo usuwamy nachodzące przedziały przed wywołaniem *)

let rec remove_interval (a, b) = function
    | Node (l, (ka, kb), r, _) ->
        let (a, b), l = if a < ka then remove_interval (a, b) l
            else (a, b), l in
        let (a, b), r = if b > kb then remove_interval (a, b) r
            else (a, b), r in
        if cmp (add_num ka (-1), add_num kb 1) (a, b) = 0
            then ((min a ka, max b kb), merge l r)
            else ((a, b), bal l (ka, kb) r)
    | Empty -> ((a, b), Empty)
(* usuwam wszystkie przedziały, które będą nachodzić na przedział po dodaniu *)

let rec add_acc x set =
    let (x, set) = (remove_interval x set) in
    add_one x set
(* pomocnicze dodawanie, aby zachować porządek w kodzie:
najpierw usuwam przedziały nachodzące, następnie dodaje konkretny przedzial *)

let rec join l v r =
    match (l, r) with
        | (Empty, _) -> add_acc v r
        | (_, Empty) -> add_acc v l
        | (Node(ll, lv, lr, (lh,_)), Node(rl, rv, rr, (rh,_))) ->
            if lh > rh + 2 then bal ll lv (join lr v r) else
            if rh > lh + 2 then bal (join l v rl) rv rr else
                make l v r

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
            let c = cmp (x,x) k in
            c = 0 || loop (if c < 0 then l else r)
        | Empty -> false in
    loop set

let exists = mem

let elements set =
    let rec loop acc = function
        | Empty -> acc
        | Node (l,k,r,_) -> loop (k :: loop acc r) l in
    loop [] set  

let split x set =
    let rec loop x = function
        | Empty -> (Empty, false, Empty)
        | Node (l, ((va, vb) as v), r, _) ->
            let c = cmp (x,x) v in
            if c = 0 then
                ((if x > va then add_acc (va, x - 1) l else l),
                true,
                (if x < vb then add_acc (x + 1, vb) r else r))
            else if c < 0 then
                let (ll, pres, rl) = loop x l in (ll, pres, join rl v r)
            else
                let (lr, pres, rr) = loop x r in (join l v lr, pres, rr) in
    loop x set

let below x set =
    let (l,pres,_) = split x set in
    add_num (count l) (if pres then 1 else 0)

(***********************
      MODYFIKATORY      
***********************)

let add = add_acc

let remove ((a,b) as x) set =
    let rec loop = function
        | Node (l, ((ka, kb) as k), r, _) ->
            let c = cmp x k in
            if c = 0 then
                let r = if b > kb then loop r else r in
                let l = if a < ka then loop l else l in
                if (ka >= a && kb <= b) then merge l r else
                if kb > b then join (if ka < a then add_acc (ka, a - 1) l else l) (b + 1, kb) r 
                else join l (ka, a - 1) r
            else if c < 0 then bal (loop l) k r else bal l k (loop r)
        | Empty -> Empty in
    loop set
(* usuwam wsyzstkie przedziały, w przedziale do usunięcia i dodaje z powrotem te kawałki,
które pozostaną po usunięciu części przedziału *)

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
