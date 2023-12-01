open Core

let input_file = "../input/day01/input.txt"




module Part_1 = struct
  let find_pairs line =
    let digits = line
    |> String.to_list
    |> List.filter_map ~f:(fun c ->
           let d = Char.to_int c - Char.to_int '0' in
           if d < 0 || d > 9 then None else Some d
         ) in
    let first = List.hd_exn digits in
    let last = List.last_exn digits in
    first * 10 + last
  let run () =
    let lines = In_channel.read_lines input_file in
    let pairs = List.map ~f:find_pairs lines in
    List.sum (module Int) pairs ~f:Fun.id
end

module Part_2 = struct
  let words = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"]
  let digits =
    List.concat [
        List.mapi ~f:(fun i word -> (i + 1, word)) words;
        List.map ~f:(fun i -> (i, Int.to_string i)) (List.range 0 10)
      ]
  let find_digit ~pos s =
    List.filter_map digits ~f:(fun (value, word) ->
        String.substr_index ~pos s ~pattern:word
        |> Option.map ~f:(fun found_pos -> (found_pos, value, found_pos + String.length word)))
    |> List.min_elt ~compare:(fun (a, _, _) (b, _, _) -> a - b)

  let rec find_digits  ?(pos=0) s =
    match (find_digit ~pos s) with
    | Some (_, value, next) -> value :: (find_digits s ~pos:next)
    | None -> []
      
  let find_pairs line = 
    let digits = find_digits line in
    let first = List.hd_exn digits in
    let last = List.last_exn digits in
    (*Printf.fprintf ch "%s,%s,%d\n" line (digits |> List.map ~f:Int.to_string |> String.concat) (first * 10 + last);*)
    first * 10 + last
  
  let run () =
    let lines = In_channel.read_lines input_file in
    (*Out_channel.with_file "dump.csv" ~f:(fun ch ->*)
    let pairs = List.map ~f:(find_pairs) lines in
    List.sum (module Int) pairs ~f:Fun.id
    (* ) *)
end
      
let part1 () =
  Printf.printf "Part 1\n";
  Printf.printf "Res = %d\n" (Part_1.run ())

let part2 () =
  Printf.printf "Part 2\n";
  Printf.printf "Res = %d\n" (Part_2.run ())  
  
let _ = part1 (); part2 ()
