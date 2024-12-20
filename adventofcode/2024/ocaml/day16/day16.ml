open Core

let input_file = "../input/day16/small-input-1.txt"


module Point = struct
  type t = (int * int) [@@deriving compare, sexp]
end


module Position = struct
  type t = (int * (int * int)) [@@deriving compare, sexp]
end

module Position_set = Set.Make(Position)


module Position_distance = struct
  type t = (int * Position.t)  [@@deriving compare, sexp]
end

module Position_distance_set = Set.Make(Position_distance)

module Part_1 = struct  
  let input () =
    let grid = In_channel.read_all input_file
               |> String.split ~on:'\n'
               |> List.map ~f:String.to_array
               |> List.to_array in
    let w = Array.length grid.(0) in
    let h = Array.length grid in
    let cp =  List.cartesian_product (List.range 0 w) (List.range 0 h) in
    let start = cp |> List.find_map_exn ~f:(fun (x, y) -> if Char.(grid.(y).(x)= 'S') then Some (1, (x, y)) else None) in
    let ending = cp |> List.find_map_exn ~f:(fun (x, y) -> if Char.(grid.(y).(x)= 'E') then Some (x, y) else None) in
    (grid, (w, h), start, ending)


  let directions = [|
    0,-1;
    1, 0;
    0, 1;
    -1, 0;
  |]

  let rotations di =
    [
      (di + 3) mod 4, 1000;
      (di + 1) mod 4, 1000;
      di, 0;
      (di + 2) mod 4, 2000;
    ]

  let _sign_to_int sign = Base__Sign0.(match sign with
    | Pos -> 1
    | Zero -> 0
    | Neg -> -1)

  let add (ax, ay) (bx, by) = (ax+bx, ay+by)

  let pop frontier =
    let me =  Position_distance_set.min_elt_exn frontier in
    let frontier =  Position_distance_set.remove frontier me in
    (frontier, me)
  
  let run () =
    let (grid, _, start, ending) = input () in
    let frontier = Position_distance_set.of_list [(0, start)] in
    let get (x, y) = grid.(y).(x) in
    let rec search frontier visited =
      let (frontier, (cel, (diel, el))) = pop frontier in
      let visited = Position_set.add visited (diel, el) in
      if Point.compare el ending = 0
      then cel
      else begin
        let next_unfiltered = List.map (rotations diel) ~f:(fun (di, cost) -> (cost + cel + 1, (di, add el (directions.(di))))) in
        let next =  next_unfiltered |> List.filter ~f:(fun (_, (di, p)) -> not (Char.(get p = '#') || (Position_set.mem visited (di, p)))) in
        let frontier = next |> List.fold_left ~init:frontier ~f:(fun acc next ->  Position_distance_set.add acc next) in
        search frontier visited
      end in
    search frontier Position_set.empty
end


module Part_2 = struct
  let input () =
    let grid = In_channel.read_all input_file
               |> String.split ~on:'\n'
               |> List.map ~f:String.to_array
               |> List.to_array in
    let w = Array.length grid.(0) in
    let h = Array.length grid in
    let cp =  List.cartesian_product (List.range 0 w) (List.range 0 h) in
    let start = cp |> List.find_map_exn ~f:(fun (x, y) -> if Char.(grid.(y).(x)= 'S') then Some (1, (x, y)) else None) in
    let ending = cp |> List.find_map_exn ~f:(fun (x, y) -> if Char.(grid.(y).(x)= 'E') then Some (x, y) else None) in
    (grid, (w, h), start, ending)


  let directions = [|
    0,-1;
    1, 0;
    0, 1;
    -1, 0;
  |]

  let rotations di =
    [
      (di + 3) mod 4, 1000;
      (di + 1) mod 4, 1000;
      di, 0;
      (di + 2) mod 4, 2000;
    ]

  let _sign_to_int sign = Base__Sign0.(match sign with
    | Pos -> 1
    | Zero -> 0
    | Neg -> -1)

  let add (ax, ay) (bx, by) = (ax+bx, ay+by)

  let pop frontier =
    let me =  Position_distance_set.min_elt_exn frontier in
    let frontier =  Position_distance_set.remove frontier me in
    (frontier, me)
  
  let run () =
    let min_cost = Part_1.run () in
    let (grid, _, start, ending) = input () in
    let frontier = Position_distance_set.of_list [(0, start)] in
    let get (x, y) = grid.(y).(x) in
    let rec search frontier visited = 
      let (frontier, (cel, (diel, el))) = pop frontier in
      let visited = Position_set.add visited (diel, el) in
      if Point.compare el ending = 0
      then cel
      else begin
        Printf.printf "Frontier.length: %d\n%!" (Position_distance_set.length frontier);
        let next_unfiltered = List.map (rotations diel) ~f:(fun (di, cost) -> (cost + cel + 1, (di, add el (directions.(di))))) in
        Printf.printf "unfiltered .length: %d\n%!" (List.length  next_unfiltered);
        let next =  next_unfiltered |> List.filter ~f:(fun (_, (di, p)) -> not (Char.(get p = '#') || (Position_set.mem visited (di, p)))) in
        Printf.printf "Next.length: %d\n%!" (List.length next);
        let frontier = next |> List.fold_left ~init:frontier ~f:(fun acc next ->  Position_distance_set.add acc next) in
        search frontier visited
      end in
    search frontier Position_set.empty
end
              
let part1 () =
  Printf.printf "\nPart 1: %d\n" (Part_1.run ())

let part2 () =
  Printf.printf "Part 2: %d\n" (Part_2.run ())
       
let _ =
  part1 ();
  part2 ()  
