open Core

let input_file = "../input/day13/small-input.txt"


module Part_1 = struct

  let input () =
    let scanner = Scanf.Scanning.from_file input_file in
    let rec loop () = 
      let av = Scanf.bscanf scanner "Button A: X+%d, Y+%d\n" (fun x y -> (x, y)) in
      let bv = Scanf.bscanf scanner "Button B: X+%d, Y+%d\n" (fun x y -> (x, y)) in
      let poa = Scanf.bscanf scanner "Prize: X=%d, Y=%d" (fun x y -> (x, y)) in
      let v =  (av, bv, poa) in
      let nl = Scanf.bscanf scanner "%[\n]%[\n]" (fun s _ -> s) in
      let rest = if String.(nl = "") then  [] else loop () in
      v::rest in
    loop ()

  
  
  let run () =
    let items = input () |> List.to_array in
    let total_cost = ref 0 in
    for i = 0 to Array.length items - 1 do
      let ((a, b), (c, d), (e, f)) = items.(i) in
      let den = a*d - c*b in
      if den = 0 then
        failwith "is parallel"
      else
        let num = d*e - c*f in
        let (num, den) = if num < 0 then (-1 * num, -1 * den) else (num, den) in
        if not (num < den || num mod den <> 0) then
          let x = num / den in
          let y = (f - b*x) / d in
          let cost = (3*x + y) in
          total_cost := !total_cost + cost 
    done;
    !total_cost
end


module Part_2 = struct
  let input () =
    let scanner = Scanf.Scanning.from_file input_file in
    let rec loop () = 
      let av = Scanf.bscanf scanner "Button A: X+%d, Y+%d\n" (fun x y -> (x, y)) in
      let bv = Scanf.bscanf scanner "Button B: X+%d, Y+%d\n" (fun x y -> (x, y)) in
      let poa = Scanf.bscanf scanner "Prize: X=%d, Y=%d" (fun x y -> (x, y)) in
      let v =  (av, bv, poa) in
      let nl = Scanf.bscanf scanner "%[\n]%[\n]" (fun s _ -> s) in
      let rest = if String.(nl = "") then  [] else loop () in
      v::rest in
    loop ()

  
  
  let run () =
    let items = input () |> List.to_array in
    let total_cost = ref 0 in
    for i = 0 to Array.length items - 1 do
      let ((a, b), (c, d), (e, f)) = items.(i) in
      let e = 10000000000000 + e in
      let f = 10000000000000 + f in
      let den = a*d - c*b in
      if den = 0 then
        failwith "is parallel"
      else
        let num = d*e - c*f in
        let (num, den) = if num < 0 then (-1 * num, -1 * den) else (num, den) in
        if not (num < den || num mod den <> 0) then
          let x = num / den in
          let y = (f - b*x) / d in
          let cost = (3*x + y) in
          total_cost := !total_cost + cost 
    done;
    !total_cost
end
              
let part1 () =
  Printf.printf "\nPart 1: %d\n" (Part_1.run ())

let part2 () =
  Printf.printf "Part 2: %d\n" (Part_2.run ())  
       
let _ =
  part1 ();
  part2 ()  



    
