open Core

let input_file = "../input/day02/input.txt"




module Part_1 = struct
  let colors = ["red", 12; "green", 13; "blue", 14] |> String.Map.of_alist_exn
  let run () =
    In_channel.read_lines input_file
    |> List.sum (module Int) ~f:(fun line ->
           let words =
             line
             |> String.filter ~f: Char.(fun c ->is_alphanum c || c = ' ')
             |> String.split ~on:' ' in
           let is_valid =
             List.zip_with_remainder (List.tl_exn words) words
             |> fst
             |> List.for_all ~f:(fun (word, count_str) ->
                    String.Map.find colors word
                    |> Option.for_all ~f:(fun max_count ->
                           (Int.of_string count_str) <= max_count)) in
           if is_valid then (List.nth_exn words 1 |> Int.of_string) else 0
         )
end

module Part_2 = struct
 let colors = ["red"; "green"; "blue"] |> String.Set.of_list
 let run () =
   In_channel.read_lines input_file
   |> List.sum (module Int) ~f:(fun line ->
          let words =
            line
            |> String.filter ~f: Char.(fun c ->is_alphanum c || c = ' ')
            |> String.split ~on:' ' in
          List.zip_with_remainder (List.tl_exn words) words
          |> fst
          |> List.filter ~f:(fun (word, _) -> String.Set.mem colors word)
          |> String.Map.of_alist_multi
          |> String.Map.to_alist
          |> List.map ~f:(fun (_, l) ->
                 List.map l ~f:Int.of_string
                 |> List.max_elt ~compare:Int.compare
                 |> Option.value_exn)
          |> List.fold ~init:1 ~f:( * ))
end
      
let part1 () =
  Printf.printf "Part 1\n";
  Printf.printf "Res = %d\n" (Part_1.run ())

let part2 () =
  Printf.printf "Part 2\n";
  Printf.printf "Res = %d\n" (Part_2.run ())  
  
let _ = part1 (); part2 ()
