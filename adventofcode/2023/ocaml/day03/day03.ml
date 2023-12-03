open Core

let input_file = "../input/day03/input.txt"

module Int_tuple = struct
  type t = (int * int) [@@deriving sexp, compare]
end

module Int_tuple_set = Set.Make(Int_tuple)
module Int_tuple_map = Map.Make(Int_tuple)   

let substrings s ~f =
  let len = String.length s in
  let rec take i c sb =
    if i >= len then
      i, (c, Buffer.contents sb)
    else
      let ch = (String.get s i) in
      if f ch then begin
          Buffer.add_char sb ch;
          take (i + 1) c  sb
        end
      else
        i, (c, Buffer.contents sb) in
  let rec drop i =
    if i >= len || f (String.get s i) then
      i
    else
      drop (i + 1) in
  let rec loop i l=
    let i = drop i in
    if i >= len then
      l
    else
      let (i, r) = take i i (Buffer.create 0) in
      loop i (r::l) in
  List.rev (loop 0 [])
                     
module Part_1 = struct
  let bounds = [-1, -1; -1, 0; -1, 1;
                 0, -1;  0, 0;  0, 1;
                 1, -1;  1, 0;  1, 1]
  let run () =
    let lines = In_channel.read_lines input_file in
    let numbers =
      List.concat_mapi lines ~f:(fun line_index line ->
          substrings line ~f:Char.is_digit
          |> List.map ~f:(fun (col_index, number_string) ->
                 (line_index, col_index, String.length number_string, Int.of_string number_string))) in
    let symbols =
      List.concat_mapi lines ~f:(fun line_index line ->
          substrings line ~f:(fun ch -> not Char.(is_digit ch || ch = '.'))
          |> List.map ~f:(fun (col_index, symbol) -> (line_index, col_index, symbol))) in
    let symbol_bounds =
      List.concat_map symbols ~f:(fun (x, y, _) -> (List.map bounds ~f:(fun (i, j) -> (i + x, j+y))))
      |> Int_tuple_set.of_list in
    let part_numbers = List.filter numbers ~f:(fun (row, col, len, _) ->
                           List.range col (col + len)
                           |> List.exists ~f:(fun col -> (Int_tuple_set.mem symbol_bounds (row, col)))) in      
    List.sum (module Int) part_numbers ~f:(fun (_, _, _, n) -> n)
end

module Part_2 = struct
  let bounds = [-1, -1; -1, 0; -1, 1;
                 0, -1;  0, 0;  0, 1;
                 1, -1;  1, 0;  1, 1]
  let run () =
    let lines = In_channel.read_lines input_file in
    let numbers =
      List.concat_mapi lines ~f:(fun line_index line ->
          substrings line ~f:Char.is_digit
          |> List.map ~f:(fun (col_index, number_string) ->
                 (line_index, col_index, String.length number_string, Int.of_string number_string))) in
    let gears =
      List.concat_mapi lines ~f:(fun line_index line ->
          substrings line ~f:(fun ch -> Char.(ch = '*'))
          |> List.map ~f:(fun (col_index, symbol) -> (line_index, col_index, symbol))) in
    let gear_bounds =
      List.concat_map gears ~f:(fun (x, y, _) ->
          (List.map bounds ~f:(fun (i, j) -> ((i + x, j + y), (x, y))) ))
      |> List.dedup_and_sort ~compare:(fun (a, _) (b, _) -> Int_tuple.compare a b)
      |> Int_tuple_map.of_alist_exn in
    let pairs = List.concat_map numbers ~f:(fun  (row, col, len, number) ->
                  List.range col (col + len)|> List.concat_map ~f:(fun col ->   
                    Int_tuple_map.find gear_bounds (row, col)
                    |> Option.map ~f:(fun gear -> (gear, number))|> Option.to_list ))
                |> Int_tuple_map.of_alist_multi
                |> Int_tuple_map.to_alist
                |> List.map ~f:snd
                |> List.map ~f:(List.dedup_and_sort ~compare:Int.compare)
                |> List.filter ~f:(fun l -> List.length l = 2) in
    List.sum (module Int) pairs  ~f:(fun pair -> List.fold pair ~init:1 ~f:( * ))
end
      
let part1 () =
  Printf.printf "Part 1\n";
  Printf.printf "Res = %d\n" (Part_1.run ())

let part2 () =
  Printf.printf "Part 2\n";
  Printf.printf "Res = %d\n" (Part_2.run ())  
  
let _ = part1 (); part2 ()
