open Core

let input_file = "../input/day06/small-input.txt"


let _winning_distances t d =
  List.range 1 (t - 1)
  |> List.map ~f:(fun pt ->
         let mt = t - pt in
         mt * pt)
  |> List.filter ~f:(fun wd -> wd > d)
  |> List.length

let winning_distances_2 t d =
  let bisect a b =
    let m = (a + b) / 2 in
    if b - a = 1 then
      if (t - a) * a > d then
        (a, a)
      else
        (b, b)
    else if (t - m) * m > d
    then
      (a, m)
    else
      (m, b) in
  let rec loop a b =
    let (a_, b_) = bisect a b in
    if a_ = b_ then a_
    else loop a_ b_ in
  let s = loop 1 (t / 2) in
  let e = t - s in
  e - s + 1
  
  
module Part_1 = struct

  let input () =
    In_channel.read_lines input_file
    |> List.map ~f:(fun line ->
           String.split line ~on:' '
           |> List.filter ~f:(fun s -> not (String.equal "" s))
           |> List.tl_exn
           |> List.map ~f:Int.of_string)
    |> List.transpose_exn
    |> List.map ~f:(fun [@warning "-8"] [a; b] -> a, b)
  


  (*
    t: total time avalable
    d: The minimum distance to travel
    pt: The time spent pressing the button
    mt: The time spent moving.

    t = pt + mt
    d > mt * pt
   *)
  let run () =
    input ()
    |> List.map ~f:(fun (t, d) -> winning_distances_2 t d)
    |> List.fold ~init:1 ~f:( * )
end

module Part_2 = struct
  let input () =
    let [@warning "-8"] [ a; b] = In_channel.read_lines input_file
    |> List.map ~f:(fun line ->
           let sp = String.Search_pattern.create " " in
           let line = String.Search_pattern.replace_all sp ~in_:line ~with_:"" in
           List.nth_exn (String.split line ~on:':') 1
           |> Int.of_string) in
    (a, b)
    
    let run () =
      let (t, d) = input () in
      winning_distances_2 t d
end
      
let part1 () =
  Printf.printf "Part 1\n";
  Printf.printf "Res = %d\n" (Part_1.run ())

let part2 () =
  Printf.printf "Part 2\n";
  Printf.printf "Res = %d\n" (Part_2.run ())  
       
let _ =
  part1 ();
  part2 ()  
