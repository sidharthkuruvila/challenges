open Core

let input_file = "../input/day09/input.txt"


module Part_1 = struct

  let input () =
    In_channel.read_lines input_file
    |> List.map ~f:(fun line -> String.split line ~on:' ' |> List.map ~f:Int.of_string)
    

  let rec triangle nums =
    if List.for_all nums ~f:(Int.equal 0) then
      [nums]
    else
      nums :: (List.zip_with_remainder nums (List.tl_exn nums)
               |> fst
               |> List.map ~f:(fun (a, b) -> b - a)
               |> triangle)

  let extrapolate nums =
    triangle nums
    |> List.map ~f:(fun nums ->  nums |> List.rev |> List.hd_exn)
    |> List.fold_right ~init:0 ~f:(fun a acc -> a + acc)
  let run () =
    input ()
    |> List.sum (module Int) ~f:extrapolate
end

module Part_2 = struct
  let input () =
    In_channel.read_lines input_file
    |> List.map ~f:(fun line -> String.split line ~on:' ' |> List.map ~f:Int.of_string)
    

  let rec triangle nums =
    if List.for_all nums ~f:(Int.equal 0) then
      [nums]
    else
      nums :: (List.zip_with_remainder nums (List.tl_exn nums)
               |> fst
               |> List.map ~f:(fun (a, b) -> b - a)
               |> triangle)

  let extrapolate nums =
    let res =
      triangle nums
      |> List.map ~f:(fun nums ->  nums |> List.hd_exn)
      |> List.fold_right ~init:0 ~f:(fun a acc -> a - acc) in
    res
    
  let run () =
    input ()
    |> List.sum (module Int) ~f:extrapolate
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



    
