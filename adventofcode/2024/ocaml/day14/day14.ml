open Core

let input_file = "../input/day14/input.txt"


module Part_1 = struct
  let read_line_opt scanner =
    Scanf.bscanf_opt scanner "p=%d,%d v=%d,%d\n" (fun px py vx vy -> ((px, py), (vx, vy)))


  let rec read_all f =
    match f () with
    | Some v -> v :: read_all f
    | None -> []
  
  let input () =
    let scanner = Scanf.Scanning.from_file input_file in
    read_all (fun () -> read_line_opt scanner)


  let run_steps (n:int) ((w, h): (int*int)) ((px, py):  (int*int)) ((vx, vy): (int*int)) : (int * int) =
    let nx = w*n + px + vx*n in
    let ny = h*n + py + vy*n in
    (nx mod w, ny mod h)
  
  let run n sz =
    let (w, h) = sz in
    let robots: ((int * int) * (int * int)) list = input () in
    let final_positions = List.map robots ~f:(fun (px, py) -> run_steps n sz px py) in
    let partitions = [
      (0,0), (w/2, h/2);
      (0,h/2+1), (w/2, h);
      (w/2+1, 0), (w, h/2);
      (w/2+1, h/2+1), (w, h)
    ] in
    List.map partitions ~f:(fun ((sz, sy), (ex, ey)) ->
        List.filter final_positions ~f:(fun (x, y) ->  sz <= x && x < ex && sy <= y && y < ey) |> List.length)
    |> List.fold ~init:1 ~f:(fun a b -> a *b)
end


module Part_2 = struct

  module Point = struct
    type t = (int * int) [@@deriving compare, sexp]
  end

  module Point_set = Set.Make(Point)
  
  let display (w, h) robots =
    let robots = List.map robots ~f:(fun (p, _) -> p) in
    let robot_set = Point_set.of_list robots in
    print_endline "";
    print_endline "";
    for j = 0 to h do
      for i = 0 to w do
        if Point_set.mem robot_set (i, j) then
          print_string "*"
        else
          print_string " "
      done;
      print_endline ""
    done
    
  
  let read_line_opt scanner =
    Scanf.bscanf_opt scanner "p=%d,%d v=%d,%d\n" (fun px py vx vy -> ((px, py), (vx, vy)))


  let rec read_all f =
    match f () with
    | Some v -> v :: read_all f
    | None -> []
  
  let input () =
    let scanner = Scanf.Scanning.from_file input_file in
    read_all (fun () -> read_line_opt scanner)
    

  let _run2 () =
    let sz =  (101, 103) in
    let (w, h) = sz in
    let next ((px, py), (vx, vy)) =
      (((w + px + vx) mod w, (h + py+ vy) mod w), (vx, vy)) in
    let rec loop n robots =
      let robots = List.map robots ~f:next in
      if n >  101*103 then
        ()
      else begin
                if n mod 101 = 12 then begin
        print_endline "==================================================================";
        print_endline (Int.to_string n);
        print_endline "==================================================================";

        display sz robots
          end;
        loop (n+1) robots
      end in
    let robots: ((int * int) * (int * int)) list = input () in
    loop 0 robots


    let _run1 () =
    let sz =  (101, 103) in
    let (w, h) = sz in
    let next ((px, py), (vx, vy)) =
      (((w + px + vx) mod w, (h + py+ vy) mod w), (vx, vy)) in
    let rec loop n robots =
      let robots = List.map robots ~f:next in
      
      if n >  101*103 then
        failwith "Ran out of steps"
      else begin
        let left = List.filter robots ~f:(fun ((px, _), _) -> px < w/2) in
        let right = List.filter robots ~f:(fun ((px, _), _) -> px > w/2) in
        if List.length left = List.length right then
          n
        else
          loop (n+1) robots
      end in
    let robots: ((int * int) * (int * int)) list = input () in
    loop 0 robots
end
              
let part1 () =
  Printf.printf "\nPart 1: %d%!\n" (Part_1.run 100 (101, 103))

let part2 () =
(*  Printf.printf "Part 2: %d\n"*) (Part_2._run2 ())
       
let _ =
  part1 ();
  part2 ()  



    
(*
   10314
10213
   10112
============

   10011


   12
113
=============
==================================================================

*)
