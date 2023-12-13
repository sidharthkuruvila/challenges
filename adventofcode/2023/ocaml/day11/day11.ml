open Core

let input_file = "../input/day11/input.txt"

module Int_pair = struct
  type t = int * int [@@deriving sexp, compare]
end
module Int_pair_set = Set.Make(Int_pair)
module Int_pair_map = Map.Make(Int_pair)
               

let input () =
  In_channel.read_lines input_file
  |> List.concat_mapi ~f:(fun y line ->
         String.to_list line 
         |> List.filter_mapi ~f:(fun x c ->
                if Char.equal c  '#' then Some (x, y)
                else None))
  |> List.mapi ~f:(fun i p -> (p, i))
  
let distance (x1, y1) (x2, y2) =
  abs(x1 - x2) + abs(y1 - y2)
  
let update_distances n l =
  List.sort l ~compare:(fun (_, a) (_, b) -> Int.compare a b)
  |> List.fold ~init:(0, 0, []) ~f:(fun (odist, ndist, l) (i, x) ->
         if odist = x then
           (odist, ndist, (i, ndist)::l)
         else
           let diff = max (x - odist - 1) 0 in
           assert (diff >= 0);
           let ndist = ndist + diff * n + 1 in
           (x, ndist, (i, ndist)::l))
  |> (fun (_, _, l) -> l)
  |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
  
let run n =
  let l = input () in
  let rows = List.map l ~f:(fun ((_, y), i) -> (i, y))
             |> update_distances n in
  let cols = List.map l ~f:(fun ((x, _), i) -> (i, x))
             |> update_distances n in
  let resized = List.map2_exn cols rows ~f:(fun (i_row, x) (i_col, y) ->
                    assert (i_row = i_col);
                    (i_row, (x, y))) in
  List.cartesian_product resized resized
  |> List.filter ~f:(fun ((i, _), (j, _)) -> i < j)
  |> List.map ~f:(fun ((_, a), (_, b)) -> distance a b
       )
  |> List.sum (module Int) ~f:Fun.id
  
let part1 () =
  Printf.printf "Part 1\n";
  Printf.printf "Res = %d\n" (run 2)

let part2 () =
  Printf.printf "Part 2\n";
  Printf.printf "Res = %d\n" (run 1000000)  
       
let _ =
  part1 ();
  part2 ()  



    
