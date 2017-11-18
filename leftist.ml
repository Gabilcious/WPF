(******************************************************
*******************************************************
             Autor: Karolina Gabara                    
             Code review: Piotr Borowski               
*******************************************************
******************************************************)

type 'a queue = Node of 'a queue * 'a * int * 'a queue | Null

exception Empty

(***********************
       POMOCNICZE       
***********************)

let depth = function
    | Null -> 0
    | Node(_,_,x,_) -> x

(***********************
      KONSTRUKTORY      
***********************)

let empty = Null

(***********************
        SELEKTORY       
***********************)

let is_empty q = q = Null

(***********************
      MODYFIKATORY      
***********************)

let rec join a b =
    match a, b with
    | q, Null
    | Null, q -> q
    | Node(_,ea,_,_), Node(_,eb,_,_) when ea > eb -> join b a
    | Node(la,ea,_,ra), b -> let r = join ra b
                              in if depth r < depth la
                                    then Node(la,ea,depth la + 1,r)
                                    else Node(r,ea,depth r + 1,la)

let add e q =
    join q (Node(Null,e,1,Null))

let delete_min = function
    | Null -> raise Empty
    | Node(lq,e,_,rq) -> (e, join lq rq)
