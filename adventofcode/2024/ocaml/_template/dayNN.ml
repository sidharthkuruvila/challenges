open Core

let input_file = "../input/dayNN/input.txt"


module Part_1 = struct  
  let input () =
    In_channel.read_all input_file
  let run () = input ()
end


module Part_2 = struct
  let input () =
    In_channel.read_all input_file
  let run () = input ()
end
              
let part1 () =
  Printf.printf "\nPart 1: %s\n" (Part_1.run ())

let part2 () =
  Printf.printf "Part 2: %s\n" (Part_2.run ())
       
let _ =
  part1 ();
  part2 ()  
