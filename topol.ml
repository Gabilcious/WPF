open List
open PMap

exception Cykliczne

let topol wej =
    let graf = List.fold_left (fun acc (v,l) -> add v l acc) empty wej in
    let st = ref empty in
    let stos = ref [] in
    let res = ref [] in
    let rec zmien a = function
        |h::t -> begin
                st := if mem h !st then add h ((find h !st) + a) !st else add h 1 !st;
                if find h !st = 0 then stos := h::!stos;
                zmien a t
            end;
        |[] -> begin end;
    in

    iter (fun node _ -> st := add node 0  !st) graf;
    iter (fun _ l -> zmien 1 l) graf;
    iter (fun node x -> if x = 0 then stos := node::!stos) !st;
    while !stos <> [] do
            let h = List.hd !stos in
			stos := List.tl !stos;			
			if mem h graf then zmien (-1) (find h graf);
			st := remove h !st;
            res := h::!res;
    done;
    if is_empty !st then List.rev !res
    else raise Cykliczne
