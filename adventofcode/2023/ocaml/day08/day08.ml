open Core

let input_file = "../input/day08/input.txt"

module Move = struct
  type t = string * char [@@deriving sexp, compare] 
end

module Move_map = Map.Make(Move)


let cartesian_product lists =
  List.fold_right lists ~init:[[]] ~f:(fun l acc -> List.cartesian_product l acc |> List.map ~f:(fun (a, b) -> a::b))

let rec gcd a b =
  if a mod b = 0 then
    b
  else
    gcd b (a mod b)

let _peek a ~f = begin
    f a;
    a
  end
  
module Part_1 = struct

  let input () =
    let [@warning "-8"] (route_s::_::map_s) = In_channel.read_lines input_file in
    let map =
      map_s
      |> List.concat_map ~f:(fun line ->
             let [@warning "-8"] [start; left; right] =
               line
               |> String.split_on_chars ~on:[' '; '('; ')'; ','; '=']
               |> List.filter ~f:String.(fun s -> s <> "") in
             [(start, 'L'), left; (start, 'R'), right]
           )
      |> Move_map.of_alist_exn in
    let route = String.to_list route_s in
    (route, map)

  let run () =
    let (route, map) = input () in
    let rec loop i l position =
      match l with
      | [] -> loop i route position
      | x::rest ->
         let next = Move_map.find_exn map (position, x) in
         if String.equal next "ZZZ" then
           i
         else
           loop (i+1) rest next in
    loop 1 route "AAA"
end

module Part_2 = struct

  let input () =
    let [@warning "-8"] (route_s::_::map_s) = In_channel.read_lines input_file in
    let map =
      map_s
      |> List.concat_map ~f:(fun line ->
             let [@warning "-8"] [start; left; right] =
               line
               |> String.split_on_chars ~on:[' '; '('; ')'; ','; '=']
               |> List.filter ~f:String.(fun s -> s <> "") in
             [(start, 'L'), left; (start, 'R'), right]
           )
      |> Move_map.of_alist_exn in
    let route = String.to_list route_s |> List.mapi ~f:(fun i c -> (i, c)) in
    let starts: string list =
      map_s
      |> List.map ~f:(String.take_while ~f:Char.(fun c -> ' ' <> c))
      |> List.filter ~f:Char.(fun s -> String.get s 2 = 'A') in
    (starts, route, map)
  let run () =
    let (starts, route, map) = input () in
    (* Given a specific starting position, find the number
       of steps it would take to get to each reachable end position
       and how many steps it would take to loop back to that end position. *)
    let rec find_loop i l position visited =
      match l with
      | [] -> find_loop i route position visited
      | (_, x)::rest ->
         let next_position = Move_map.find_exn map (position, x) in
         if Char.(String.get next_position 2 = 'Z') then
           if String.Map.mem visited next_position then 
             let loop_length = i -  String.Map.find_exn visited next_position in
             (next_position, loop_length, visited |> String.Map.to_alist)
           else
             find_loop (i + 1) rest next_position (String.Map.add_exn visited ~key:next_position ~data:i)
         else
           find_loop (i+1) rest next_position visited in
    
    let loops = List.map starts ~f:(fun start ->
                    let  (_, loop_length, visited) = find_loop 1 route start String.Map.empty in
                    List.map visited ~f:(fun (position, index) -> (position, index, loop_length))) in
    let rows: (string * int * int) list list =  cartesian_product loops  in
    rows
    |> List.map ~f:(fun row ->
           row |> List.map ~f:(fun (_, _, a) -> a)
           |> List.fold ~init:1 ~f:(fun a b ->
                  let g = gcd a b in (a * (b/g))))
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



    
