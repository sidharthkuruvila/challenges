open Core

let input_file = "../input/day15/input.txt"


let sections_re = Re.Pcre.re "\n\n" |> Re.compile
let eol_re = Re.Pcre.re "\n" |> Re.compile


module Part_1 = struct  
  let input () =
    let s = In_channel.read_all input_file in
    let [@warning "-8"] [grid_s; op_lines] = Re.split sections_re s in
    let grid = String.split grid_s ~on:'\n' |> List.map ~f:(fun line -> String.to_array line) |> List.to_array in
    let ops = Re.replace_string eol_re ~by:"" op_lines in
    let w = Array.length grid.(0) in
    let h = Array.length grid in
    (h, w, grid, ops |> String.to_list)

  let display grid =
    let s = Array.to_list grid |> List.map ~f:(fun row -> row |> Array.to_list |> List.map ~f:(Char.to_string) |> String.concat) |> String.concat ~sep:"\n" in
    Printf.printf "%s\n%!" s
  
  let run () =
    let directions = [
      '^', (0,-1);
      '>', (1,0);
      'v', (0,1);
      '<',(-1,0);   
    ] |> Char.Map.of_alist_exn  in
    let (h, w, grid, ops) = input () in
    let get (x, y) = grid.(y).(x) in
    let set (x, y) c = grid.(y).(x) <- c in
    let add (ax, ay) (bx, by) = (ax+bx, ay+by) in
    let score () =
      List.cartesian_product (List.range 0 w) (List.range 0 h)
      |> List.filter ~f:(fun p -> Char.(get p = 'O')) |> List.sum (module Int) ~f:(fun (x, y) -> 100*y+x) in
    let try_move robot direction =
      let rec find_empty p path=
        let np = add p direction in
        let c = get np in
        if Char.(c = '#') then
          None
        else if Char.(c = '.') then
            Some (np::path)
        else
          find_empty np (np::path) in
      let rec move path =
        match path with
        | p1::p2::rest ->
          set p1 (get p2);
          move (p2::rest)
        | p::[] ->
          set p '.'
        | [] -> failwith "Should not be empty" in
      match find_empty robot [robot] with
      | Some path ->
        move path;
        add robot direction
      | None ->
        robot in
          

    let robot = List.cartesian_product (List.range 0 w) (List.range 0 h)
                |> List.find_map_exn ~f:(fun p ->  if Char.(get p = '@') then Some p  else None) in
    let rec loop robot ops =
      match ops with
      | [] -> ()
      | op::rest ->
        (* let robot_old = robot in*)
        let robot = try_move robot (Char.Map.find_exn directions op) in
        (*Printf.printf "\n\nDirection: %c, robot: '%c'(%d, %d) -> '%c'(%d, %d) \n" op (get robot_old) (fst robot_old) (snd robot_old)  (get robot) (fst robot) (snd robot);
          display grid;*)
        loop robot rest in
    (*display grid;
      Printf.printf "\n\n\n";*)
    ignore (loop robot ops);
    display grid;
    score ()
end


module Part_2 = struct


  module Point = struct
    type t = (int * int) [@@deriving compare, sexp]
  end

  module Point_set = Set.Make(Point)
  
  let double_width grid =
    Array.map grid ~f:(fun line ->
        Array.to_list line
        |> List.map ~f:(fun c ->
            match c with
            | '.' -> ".."
            | '#' -> "##"
            | 'O' -> "[]"
            | '@' -> "@."
            | _ -> failwith "Unknown char")
        |> String.concat |> String.to_array)
      
  let input () =
    let s = In_channel.read_all input_file in
    let [@warning "-8"] [grid_s; op_lines] = Re.split sections_re s in
    let grid = String.split grid_s ~on:'\n' |> List.map ~f:(fun line -> String.to_array line) |> List.to_array |> double_width in
    let ops = Re.replace_string eol_re ~by:"" op_lines in
    let w = Array.length grid.(0) in
    let h = Array.length grid in
    (h, w, grid, ops |> String.to_list)

  let display grid =
    let s = Array.to_list grid |> List.map ~f:(fun row -> row |> Array.to_list |> List.map ~f:(Char.to_string) |> String.concat) |> String.concat ~sep:"\n" in
    Printf.printf "%s\n%!" s
  
  let run () =
    let directions = [
      '^', (0,-1);
      '>', (1,0);
      'v', (0,1);
      '<',(-1,0);   
    ] |> Char.Map.of_alist_exn  in
    let (h, w, grid, ops) = input () in
    let get (x, y) = grid.(y).(x) in
    let set (x, y) c = grid.(y).(x) <- c in
    let add (ax, ay) (bx, by) = (ax+bx, ay+by) in
    let score () =
      List.cartesian_product (List.range 0 w) (List.range 0 h)
      |> List.filter ~f:(fun p -> Char.(get p = '[')) |> List.sum (module Int) ~f:(fun (x, y) -> 100*y+x) in
    let try_move_1 robot direction =
      let rec find_empty p path=
        let np = add p direction in
        let c = get np in
        if Char.(c = '#') then
          None
        else if Char.(c = '.') then
            Some (np::path)
        else
          find_empty np (np::path) in
      let rec move path =
        match path with
        | p1::p2::rest ->
          set p1 (get p2);
          move (p2::rest)
        | p::[] ->
          set p '.'
        | [] -> failwith "Should not be empty" in
      match find_empty robot [robot] with
      | Some path ->
        move path;
        add robot direction
      | None ->
        robot in


    let try_move_2 robot direction =
      let rec  find_moves p moves =
        let np = add p direction in
        let c = get np in
        match c with
        | '#' -> None
        | '.' -> Some([p, np])
        | '[' ->
          let a = find_moves np moves in
          let b = find_moves (add np (1, 0)) moves in
          if Option.is_none a || Option.is_none b then
            None
          else
            Some ((p, np)::List.concat [Option.value_exn a; Option.value_exn b])
        | ']' ->
          let a = find_moves np moves in
          let b = find_moves (add np (-1, 0)) moves in
          if Option.is_none a || Option.is_none b then
            None
          else
            Some ((p, np)::List.concat [Option.value_exn a; Option.value_exn b])
        | _ -> failwith "Unknown character" in
      match find_moves robot [] with
      | Some moves ->
        let rec loop seen moves =
          match moves with
          | [] -> ()
          | (p, np)::rest -> if not (Point_set.mem seen p) then begin
              set np (get p); (set p '.')
            end;
            loop (Point_set.add seen p) rest in
        loop Point_set.empty (List.rev moves);
        set robot '.';
        add robot direction
      | None ->
        robot in
            
    
    let robot = List.cartesian_product (List.range 0 w) (List.range 0 h)
                |> List.find_map_exn ~f:(fun p ->  if Char.(get p = '@') then Some p  else None) in
    let rec loop robot ops =
      match ops with
      | [] -> ()
      | op::rest ->
        (*let robot_old = robot in*)
        let robot =
          if Char.(op = '<' || op = '>') then
            try_move_1 robot (Char.Map.find_exn directions op)
          else
            try_move_2 robot (Char.Map.find_exn directions op) in
        (*Printf.printf "\n\nDirection: %c, robot: '%c'(%d, %d) -> '%c'(%d, %d) \n" op (get robot_old) (fst robot_old) (snd robot_old)  (get robot) (fst robot) (snd robot);
          display grid;*)
        loop robot rest in
    display grid;
    (*Printf.printf "\n\n\n";*)
    ignore (loop robot ops);
    display grid;
    score ()
end
              
let part1 () =
  Printf.printf "\nPart 1: %d\n" (Part_1.run ())

let part2 () =
  Printf.printf "Part 2: %d\n" (Part_2.run ())
       
let _ =
  part1 ();
  part2 ()  
