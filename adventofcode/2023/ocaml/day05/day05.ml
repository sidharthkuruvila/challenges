open Core

let input_file = "../input/day05/input.txt"

module Part_1 = struct
  let run () =
    let [@warning "-8"] seeds_line::map_strings =
      In_channel.read_all input_file
      |> String.Search_pattern.(split_on ( create "\n\n")) in
    let seeds =
      seeds_line
      |> String.split ~on:' '
      |> List.tl_exn
      |> List.map ~f:Int.of_string in
    let maps = map_strings
               |> List.map ~f:(fun map_string ->
                      map_string
                      |> String.strip
                      |> String.split ~on:'\n'
                      |> List.tl_exn
                      |> List.map ~f:(fun line ->
                             let [@warning "-8"] [a; b; c] =
                               line                              
                               |> String.split ~on:' '
                               |> List.map ~f:Int.of_string in
                             (a, b,c))) in
    let apply_map seeds map =
      List.map seeds ~f:(fun seed ->
          List.find map ~f:(fun (_, s, l) ->  s <= seed && seed < s + l )
          |> Option.value_map ~f:(fun (d, s, _) -> d - s + seed) ~default:seed) in
    List.fold_left ~init:seeds ~f:apply_map maps
    |> List.min_elt ~compare:Int.compare
    |> Option.value_exn  
end

module Part_2 = struct
  let apply_map_to_seed map seed =
    let rec loop l ((seed_start, seed_length) as seed) =
      match l with
      | _ when seed_length <= 0 -> []
      | [] -> [seed]      
      | (_, s, _)::_ when seed_start + seed_length <= s -> [seed]
      | (_, s, _)::_ when seed_start < s ->
         (seed_start, s - seed_start)::(loop l (s, seed_length - s + seed_start))
      | (_, s, len)::rest when seed_start >= s + len -> loop rest seed
      | (d, s, len)::rest when seed_start >= s ->
         let e = seed_start - s in
         assert (len >= e);
         if seed_length < len - e then
           [(d + e, seed_length)]
         else
         (d + e, len - e)::(loop rest (s + len, seed_length + e - len))
      | (d, s, l)::_ ->
         failwith (Printf.sprintf "Shouldn't come here d:%d, s:%d, l:%d - seed_start:%d, seed_length:%d"
                     d s l seed_start seed_length) in
    loop map seed

  let apply_map seeds map =
    List.concat_map seeds ~f:(apply_map_to_seed map)

 let run () =
    let [@warning "-8"] seeds_line::map_strings =
      In_channel.read_all input_file
      |> String.Search_pattern.(split_on ( create "\n\n")) in
    let seeds = seeds_line
                |> String.split ~on:' '
                |> List.tl_exn
                |> List.map ~f:Int.of_string
                |> List.groupi ~break:(fun i _ _ -> (i mod 2) = 0)
                |> List.map ~f:(fun [@warning "-8"] [a;b] -> (a, b)) in
    let maps = map_strings
               |> List.map ~f:(fun map_string ->
                      map_string
                      |> String.strip
                      |> String.split ~on:'\n'
                      |> List.tl_exn
                      |> List.map ~f:(fun line ->
                             let [@warning "-8"] [a; b; c] =
                               line                              
                               |> String.split ~on:' '
                               |> List.map ~f:Int.of_string in
                             (a, b,c))
                      |> List.sort ~compare:(fun (_, a, _) (_, b, _) -> Int.compare a b)) in
    List.fold_left ~init:seeds ~f:apply_map maps
    |> List.map ~f:(fun (a, _) -> a)
    |> List.min_elt ~compare:Int.compare
    |> Option.value_exn
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
