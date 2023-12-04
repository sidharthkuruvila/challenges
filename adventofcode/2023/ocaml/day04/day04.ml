open Core

let input_file = "../input/day04/input.txt"

module Part_1 = struct

  let run () =
      In_channel.read_lines input_file
      |> List.map ~f:(fun line ->
             let [@warning "-8"] [a; b] =
               String.sub line ~pos:8 ~len:(String.length line - 8)
               |> String.split ~on:'|'
               |> List.map ~f:(fun part ->
                      String.split ~on:' ' part
                      |> List.filter ~f:String.(fun s -> s <> "")
                      |> String.Set.of_list) in
             let count = String.Set.inter a b |> String.Set.length in
             1 lsl (count - 1)
           )
      |> List.sum (module Int) ~f:Fun.id
    
end

module Part_2 = struct
  let run () =
    let counts =
      In_channel.read_lines input_file
      |> List.map ~f:(fun line ->
             let [@warning "-8"] [a; b] =
               String.sub line ~pos:8 ~len:(String.length line - 8)
               |> String.split ~on:'|'
               |> List.map ~f:(fun part ->
                      String.split ~on:' ' part
                      |> List.filter ~f:String.(fun s -> s <> "")
                      |> String.Set.of_list) in
             String.Set.inter a b |> String.Set.length) in
    let card_counts = List.map counts ~f:(fun c -> (c, 1)) in
    let rec loop l =
      let rec incn l n m=
        match l with
        | (c, i)::xs when n > 0 -> (c, i+m)::incn xs (n-1) m
        | _ -> l in
      match l with
      | (c, i)::xs -> i + loop (incn xs c i)
      | [] -> 0 in
    loop card_counts
end
      
let part1 () =
  Printf.printf "Part 1\n";
  Printf.printf "Res = %d\n" (Part_1.run ())

let part2 () =
  Printf.printf "Part 2\n";
  Printf.printf "Res = %d\n" (Part_2.run ())  
  
let _ = part1 (); part2 ()
