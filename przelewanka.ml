let rec gcd a b =
        if b = 0 then a else gcd b (a mod b)
    
let przelewanka tab =
    let n = Array.length tab in
    if n = 0 then 0 else
    let vis = Hashtbl.create n and
    bfs = Queue.create () and
    res = ref (-1) and
    final = Array.init n (fun i -> snd tab.(i)) in
    
    let addQue x steps =
        if !res = -1 && not (Hashtbl.mem vis x) then begin
            if x = final then res := steps + 1
            else begin
                Queue.add (Array.copy x) bfs;
                Hashtbl.add vis (Array.copy x) (steps + 1);
            end
        end in
    let possible = ref (Array.fold_left (fun acc (x,y) -> if x = y || y = 0 then false else acc) true tab) in
    let toFasten = max 1 (Array.fold_left max 0 (Array.map fst tab)) in
    let nwd = Array.fold_left (fun acc (x, _) -> gcd acc x) (toFasten) tab in
    Array.iter (fun (_,y) -> if (y mod nwd) <> 0 then possible := true) tab;
    
    if n = 0 then 0
    else if !possible then -1
    else begin
        addQue (Array.make n 0) (-1);
        while !res = -1 && not (Queue.is_empty bfs) do
            let el = Queue.pop bfs in
            let steps = Hashtbl.find vis el in
            for i = 0 to (n-1) do
                let last = el.(i) in
                el.(i) <- 0;
                addQue el steps;
                el.(i) <- fst tab.(i);
                addQue el steps;
                for j = 0 to (n-1) do
                    if j<>i then begin
                        let last2 = el.(j) in
                        el.(i) <- max (el.(j) + last - (fst tab.(j))) 0;
                        el.(j) <- min (el.(j) + last) (fst tab.(j));
                        addQue el steps;
                        el.(j) <- last2;
                    end
                done;
                el.(i) <- last;
            done;
        done;
        !res
    end
