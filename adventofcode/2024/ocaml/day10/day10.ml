open Core

let input_file = "../input/day10/input.txt"


module Part_1 = struct

  let input () =
    In_channel.read_all input_file
    |> String.split_lines
    |> List.map ~f:(fun s -> String.to_array s |> Array.map ~f:(fun ch -> Char.to_int ch - 48))
    |> List.to_array



  module Point = struct
    type t = (int * int) [@@deriving compare, sexp]
  end

  module Point_set = Set.Make(Point)
  
  let run () =
    let grid = input () in
    let width = Array.length grid.(0) in
    let height = Array.length grid in
    let in_bounds (x, y) = 0 <= x && x < width && 0 <= y && y < height in
    let moves = [
      -1, 0;
      1, 0;
      0, -1;
      0, 1
    ] in
    let steps: int list = List.range 1 10 in
    let trail_heads =
      List.cartesian_product (List.range 0 width) (List.range 0 height)
      |> List.filter ~f:(fun (x, y) -> grid.(y).(x) = 0) in
    let next_step frontier next =
      Point_set.to_list frontier
          |> List.concat_map ~f:(fun (x, y) ->
              List.filter_map moves ~f:(fun (i, j) ->
                  let nx = x + i in
                  let ny = y + j in
                  if in_bounds (nx, ny) && grid.(ny).(nx) = next then Some (nx, ny)
                  else None)) |> Point_set.of_list in 
    let end_points trail_head = List.fold_left steps
      ~init:(Point_set.of_list [trail_head])
      ~f:next_step in
    List.sum (module Int) trail_heads ~f:(fun trail_head -> end_points trail_head |> Point_set.length)
end


module Part_2 = struct
  let input () =
    In_channel.read_all input_file
    |> String.split_lines
    |> List.map ~f:(fun s -> String.to_array s |> Array.map ~f:(fun ch -> Char.to_int ch - 48))
    |> List.to_array



  module Point = struct
    type t = (int * int) [@@deriving compare, sexp]
  end

  module Point_set = Set.Make(Point)
  
  let run () =
    let grid = input () in
    let width = Array.length grid.(0) in
    let height = Array.length grid in
    let in_bounds (x, y) = 0 <= x && x < width && 0 <= y && y < height in
    let moves = [
      -1, 0;
      1, 0;
      0, -1;
      0, 1
    ] in
    let steps: int list = List.range 1 10 in
    let trail_heads =
      List.cartesian_product (List.range 0 width) (List.range 0 height)
      |> List.filter ~f:(fun (x, y) -> grid.(y).(x) = 0) in
    let next_step frontier next =
      frontier
          |> List.concat_map ~f:(fun (x, y) ->
              List.filter_map moves ~f:(fun (i, j) ->
                  let nx = x + i in
                  let ny = y + j in
                  if in_bounds (nx, ny) && grid.(ny).(nx) = next then Some (nx, ny)
                  else None)) in 
    let end_points trail_head = List.fold_left steps
      ~init:([trail_head])
      ~f:next_step in
    List.sum (module Int) trail_heads ~f:(fun trail_head -> end_points trail_head |> List.length)
end
              
let part1 () =
  Printf.printf "\nPart 1: %d\n" (Part_1.run ())

let part2 () =
  Printf.printf "Part 2: %d\n" (Part_2.run ())  
       
let _ =
  part1 ();
  part2 ()  



    
