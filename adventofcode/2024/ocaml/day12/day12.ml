open Core

let input_file = "../input/day12/input.txt"


module Part_1 = struct

  module Point = struct
    
    type t = (int * int) [@@deriving compare, sexp]
  end

  module Point_set = Set.Make(Point)

  let input () =
    let grid = In_channel.read_all input_file |> String.split_lines |> List.map ~f: String.to_array |> List.to_array in
    let width = Array.length grid.(0) in
    let height = Array.length grid in
    (grid, width, height)
    
  
  let run () =

    let (grid, width, height) = input () in
    let points = List.cartesian_product (List.range 0 width) (List.range 0 height) |> Point_set.of_list in
    let rec loop points =
      if Point_set.length points = 0 then
        0
      else
        let directions = [0, 1; 0, -1; -1, 0; 1, 0] in
        let (x, y) = Point_set.min_elt_exn points in
        let ch = grid.(y).(x) in
        let rec dimensions frontier (points: Point_set.t) =
          if List.length frontier = 0 then
            (1, 0, points)
          else
            let points = List.fold_left frontier  ~init:points ~f:(fun points pt -> Point_set.remove points pt) in
            let nexts = List.concat_map frontier ~f:(fun (x, y) -> List.filter_map directions ~f:(fun (i, j) ->
                let nx = x + i in
                let ny = y + j in
                if 0 <= nx && nx < width && 0 <= ny && ny < height && Char.(ch = grid.(ny).(nx))  then
                (if Point_set.mem points (nx, ny) then
        
                   Some (true, nx, ny)
                     else None)
                else
                  Some (false, nx, ny))) in
            let (frontier, borders) = List.partition_map nexts ~f:(fun (is_inside, x, y) -> if is_inside then (Either.First (x, y)) else (Either.Second (x, y))) in
            let frontier = List.dedup_and_sort frontier ~compare:Point.compare in
            let dl = List.length borders in
            let da = List.length frontier in
            let (a, l, points) = dimensions frontier points in
            (a+da, l + dl, points) in
        let (a, l, points) = dimensions [(x,y)] points in
        a*l + (loop points) in
    loop points

end


module Part_2 = struct
  module Point = struct
    
    type t = (int * int) [@@deriving compare, sexp]
  end

  module Point_set = Set.Make(Point)

  let input () =
    let grid = In_channel.read_all input_file |> String.split_lines |> List.map ~f: String.to_array |> List.to_array in
    let width = Array.length grid.(0) in
    let height = Array.length grid in
    (grid, width, height)
    
  
  let run () =

    let (grid, width, height) = input () in
    let points = List.cartesian_product (List.range 0 width) (List.range 0 height) |> Point_set.of_list in
    let to_index width (x, y) = (y+1)*(width+1)+x in
    let rec loop points =
      if Point_set.length points = 0 then
        0
      else
        let directions = [0, 1; 0, -1; -1, 0; 1, 0] in
        let (x, y) = Point_set.min_elt_exn points in
        let ch = grid.(y).(x) in
        let rec dimensions frontier (points: Point_set.t) =
          if List.length frontier = 0 then
            (1, [], points)
          else
            let points = List.fold_left frontier  ~init:points ~f:(fun points pt -> Point_set.remove points pt) in
            let nexts = List.concat_map frontier ~f:(fun (x, y) -> List.filter_mapi directions ~f:(fun n (i, j) ->
                let nx = x + i in
                let ny = y + j in
                if 0 <= nx && nx < width && 0 <= ny && ny < height && Char.(ch = grid.(ny).(nx))  then
                (if Point_set.mem points (nx, ny) then
                   Some (true, nx, ny, n)
                     else None)
                else
                  Some (false, nx, ny, n))) in
            let (frontier, borders) = List.partition_map nexts ~f:(fun (is_inside, x, y, n) -> if is_inside then (Either.First (x, y)) else (Either.Second (x, y, n))) in
            let frontier = List.dedup_and_sort frontier ~compare:Point.compare in
            let da = List.length frontier in
            let (a, l, points) = dimensions frontier points in
            (a+da, List.concat [l; borders], points) in
        let (a, borders, points) = dimensions [(x,y)] points in


        let operators =
          [
            0, (fun (x, y) -> to_index height (x, y));
            1, (fun (x, y) -> to_index height (x, y));
            2, (fun (x, y) -> to_index width (y, x));
            3, (fun (x, y) -> to_index width (y, x));
          ] in
        let l = List.sum (module Int) operators ~f:(fun (n_, fn) -> (List.filter_map borders ~f:(fun (x, y, n) -> if n = n_ then Some (fn (x, y)) else None)) |> List.dedup_and_sort ~compare:Int.compare |> List.group ~break:(fun i1 i2 -> Int.abs(i1 - i2) > 1) |> List.length) in
        a*l + (loop points) in
    loop points
end
              
let part1 () =
  Printf.printf "\nPart 1: %d\n" (Part_1.run ())

let part2 () =
  Printf.printf "Part 2: %d\n" (Part_2.run ())  
       
let _ =
  part1 ();
  part2 ()  



    
