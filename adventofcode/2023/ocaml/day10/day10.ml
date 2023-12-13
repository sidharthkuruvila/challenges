open Core

let input_file = "../input/day10/input.txt"

module Int_pair = struct
  type t = int * int [@@deriving sexp, compare]
end

module Int_pair_map = Map.Make(Int_pair)

module Int_pair_set = Set.Make(Int_pair)


let rec bit_pos n =
  if n = 0
  then 0   
  else if n = 1
  then 0
  else 1 + bit_pos (n lsr 1)

let _direction_to_string d =
  [|"N";"W";"S";"E"|].(d)
                    
module Part_1 = struct
  let start = 1 lsl 4
  let north = 1 lsl 0
  let west = 1 lsl 1
  let south = 1 lsl 2
  let east = 1 lsl 3

  let directions = [|0,-1;-1,0;0,1;1,0|]
  let alternate = [|south;east;north;west|]

  let input () =
    In_channel.read_lines input_file
    |> List.concat_mapi ~f:(fun y line ->
           String.to_list line 
           |> List.mapi ~f:(fun x c ->
                  let p = match c with
                  | 'S' -> start
                  | '|' -> north lor south
                  | '-' -> east lor west
                  | 'L' -> north lor east
                  | 'J' -> north lor west
                  | '7' -> south lor west
                  | 'F' -> south lor east
                  | '.' -> 0
                  | _ -> failwith "Nothing else matches" in
                  ((x, y), p)))
    |> List.filter ~f:(fun (_, m) -> not (m = 0))
    

  let run () =
    let l =  input () in
    let ((sx, sy), _) = List.find_exn l ~f:(fun (_, m) -> m = start) in 
    let positions = l |> Int_pair_map.of_alist_exn in
    let  [@warning "-8"] [|start_pipe, start_port; end_pipe, _|] =
      directions
      |> Array.filter_mapi ~f:(fun n (i, j) ->
             Int_pair_map.find positions (sx + i, sy + j)
             |> Option.find_map ~f:(fun m ->
                    if m land alternate.(n) <> 0
                    then Some ((sx+i, sy+j),  alternate.(n))
                    else None)) in
    
    let rec loop_length pipe previous_port len =
      if Int_pair.compare pipe end_pipe = 0 then
        len
      else
        let ports = Int_pair_map.find_exn positions pipe in
        let next_port = ports land (lnot previous_port) in
        let next_port_direction_i = bit_pos next_port in
        let (i, j) = directions.(next_port_direction_i) in
        let (x, y) = pipe in
        let next_pipe = (x + i, y + j) in
        loop_length next_pipe alternate.(next_port_direction_i) (len + 1) in
    (loop_length start_pipe start_port 2) /2  
end

module Part_2 = struct
  let start = 1 lsl 4
  let north = 1 lsl 0
  let west = 1 lsl 1
  let south = 1 lsl 2
  let east = 1 lsl 3

  let directions = [|0,-1;-1,0;0,1;1,0|]
  let alternate = [|south;east;north;west|]

  let input () =
    let lines = In_channel.read_lines input_file in
    let positions = lines |> List.concat_mapi ~f:(fun y line ->
           String.to_list line 
           |> List.mapi ~f:(fun x c ->
                  let p = match c with
                  | 'S' -> start
                  | '|' -> north lor south
                  | '-' -> east lor west
                  | 'L' -> north lor east
                  | 'J' -> north lor west
                  | '7' -> south lor west
                  | 'F' -> south lor east
                  | '.' -> 0
                  | _ -> failwith "Nothing else matches" in
                  ((x, y), p))) in
    (positions, (List.hd_exn lines |> String.length, List.length lines))
      
  
  let run () =
    let (l, (w, h)) =  input () in
    let ((sx, sy), _) = List.find_exn l ~f:(fun (_, m) -> m = start) in 
    let positions = l |> Int_pair_map.of_alist_exn in
    let  [@warning "-8"] [|start_pipe, start_port; end_pipe, end_port|] =
      directions
      |> Array.filter_mapi ~f:(fun n (i, j) ->
             Int_pair_map.find positions (sx + i, sy + j)
             |> Option.find_map ~f:(fun m ->
                    if  m land alternate.(n) <> 0
                    then Some ((sx+i, sy+j),  alternate.(n))
                    else None)) in
    let rec find_loop pipe previous_port acc =
      if Int_pair.compare pipe end_pipe = 0 then
        acc
      else
        let ports = Int_pair_map.find_exn positions pipe in
        let next_port = ports land (lnot previous_port) in
        let next_port_direction_i = bit_pos next_port in
        let (i, j) = directions.(next_port_direction_i) in
        let (x, y) = pipe in
        let next_pipe = (x + i, y + j) in
        find_loop next_pipe alternate.(next_port_direction_i) (pipe::acc) in
    let loop_pipes =
      start_pipe::(sx, sy)::end_pipe::(find_loop start_pipe start_port [])
      |> Int_pair_set.of_list in
    let s_ports = alternate.(bit_pos start_port) lor alternate.(bit_pos end_port) in
    List.sum (module Int) (List.range 0 h) ~f:(fun y ->
           let r = List.range 0 w
           |> List.fold ~init:(false, 0, 0) ~f:(fun (in_loop, last_dir, count) x ->
                  if Int_pair_set.mem loop_pipes (x, y)
                  then
                    let ports_ = Int_pair_map.find_exn positions (x, y) in
                    let ports = if ports_ = 16 then s_ports else ports_ in
                    if ports = west lor east then begin
                      Printf.printf "-";
                      (in_loop, last_dir, count)
                      end else if ports = north lor south then begin
                        Printf.printf "|";
                        (not in_loop, 0, count)
                      end else if ports land east = 0 then begin
                         Printf.printf "3";
                        ((if last_dir land ports <> 0  then in_loop else not in_loop) , 0, count)
                      end else begin
                         Printf.printf "E";
                        (in_loop, ports land (lnot east), count)
                      end
                  else if in_loop then begin
                    Printf.printf ".";
                    (in_loop, 0, count + 1)
                    end else begin
                      Printf.printf "o";
                      (in_loop, 0, count)
                    end
                ) |> (fun (_, _, count) -> count) in
           Printf.printf "\n";
           r)
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



    
