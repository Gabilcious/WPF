let przelewanka tab =
    let n = Array.length tab in
    let vis = Hashtbl.create n and
    bfs = Queue.create () and
    res = ref (-1) and
    final = Array.init n (fun i -> snd tab.(i)) in
    
    if n = 0 then 0
    else if Array.fold_left
            (fun acc (x,y) -> if y = 0 || x = y then true else acc) false tab then -1
    else begin
        let addQue x steps =
            if !res = -1 && not (Hashtbl.mem vis x) then begin
                if x = final then res := steps + 1
                else begin
                    Queue.add x bfs;
                    Hashtbl.add vis x (steps + 1); (*Array.copy x ???*)
                end
            end in
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
                        el.(j) <- min (el.(j) + last) (fst tab.(j));
                        el.(i) <- max (el.(j) + last - (fst tab.(j))) 0;
                        addQue el steps;
                        el.(j) <- last2;
                    end
                done;
                el.(i) <- last;
            done;
        done;
        !res
    end
