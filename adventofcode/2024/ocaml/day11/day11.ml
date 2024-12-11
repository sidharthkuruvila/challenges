open Core

let input_file = "../input/day11/input.txt"


module Part_1 = struct

  let input () =
    In_channel.read_all input_file |> String.split ~on:' ' |> List.map ~f:Int.of_string



  let step n =
    if n = 0 then [1]
    else
      let ns = Int.to_string n in
      let nsl = String.length ns in
      if nsl mod 2 = 0 then
        let nslh = nsl/2 in
        let part1 = String.sub ns ~pos:0 ~len:nslh in
        let part2 = String.sub ns ~pos:nslh ~len:nslh in
        [part1; part2] |> List.map ~f:Int.of_string
      else
        [n * 2024]
  
  let run count =
    let initial = input () |> List.map ~f:(fun n -> (n, 1)) |> Int.Map.of_alist_reduce ~f:(+) in
    let rec loop n acc =
      if n = 0 then
        acc
      else
        let items = Int.Map.to_alist acc in
        let new_nums = List.concat_map items ~f:(fun (n, c) -> step n |> List.map ~f:(fun nn -> (nn, c))) in
        loop (n-1) (Int.Map.of_alist_reduce new_nums ~f:(+)) in
    let final = loop count initial in
    Int.Map.to_alist final |> List.sum (module Int) ~f:(fun (_, c) -> c)
end

              
let part1 () =
  Printf.printf "\nPart 1: %d\n" (Part_1.run 24)

let part2 () =
  Printf.printf "Part 2: %d\n" (Part_1.run 75)  
       
let _ =
  part1 ();
  part2 ()  



    
