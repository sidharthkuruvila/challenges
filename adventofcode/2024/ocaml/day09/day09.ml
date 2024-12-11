open Core

let input_file = "../input/day09/input.txt"




module Part_1 = struct

  let input () =
    In_channel.read_all input_file
    |> String.to_list |> List.map ~f:(fun c -> Char.to_int c - 48)
    

  let make_intermediate l =
    let rec loop l i index =
      match l with
      | a::b::rest ->
        (i, a, index, b)::loop rest (i+1) (index+a+b)
      | [a] -> [(i, a, index, 0)]
      | _ -> failwith "No" in
    loop l 0 0

let rec pack_steps fronts backs =
  let [@warning "-8"] (fi, _, findex, fg)::frst = fronts in
  let [@warning "-8"] (bi, bs, bindex, _)::brst = backs in
  if fi = bi then
    []
  else if bs = 0 then
    pack_steps fronts brst
  else if fg = 0 then
    let [@warning "-8"] (ni, ns, nindex, ng)::nrst = frst in
    if ni = bi then
      [(bi, bs, bindex)]
    else
      (ni, ns, nindex)::pack_steps ((ni, 0, nindex+ns, ng)::nrst) backs
  else
    let diff = Int.min fg bs in
    (bi, diff, findex)::pack_steps ((fi, 0, findex + diff, fg - diff)::frst) ((bi, bs-diff, bindex, 0)::brst)
    
    
let pack l =
  let fronts = (0, 0, 0, 0)::l in
  let backs = List.rev l in
  pack_steps fronts backs


let rec check_sum (i, n, index) =
  if n = 0 then
    0
  else
    i*index + check_sum(i, n-1, index+1)
  
let run () =
  input () |>  make_intermediate |> pack |> List.sum (module Int) ~f:check_sum
end


module Part_2 = struct

  (*let display l =
    let rec fill_zeros start l =
      match l with
      | (_, _, index)::_ ->
        Printf.printf "%s" (String.make (index - start) '.');
        fill_num l
      | [] -> ()
    and fill_num l =
      match l with
      | (i, n, index)::rst ->
        Printf.printf "%s" (String.make n (Char.of_int_exn (48 + i)));
        fill_zeros (index + n) rst
      | [] -> () in
    fill_zeros 0 l;
    print_endline ""*)
    
  
  let input () =
    In_channel.read_all input_file
    |> String.to_list |> List.map ~f:(fun c -> Char.to_int c - 48)
    

  let make_intermediate l =
    let rec loop l i index =
      match l with
      | a::b::rest ->
        (i, a, index, b)::loop rest (i+1) (index+a+b)
      | [a] -> [(i, a, index, 0)]
      | _ -> failwith "No" in
    loop l 0 0

  
  let fill_gaps back gaps =
    let gap_map = List.map gaps ~f:(fun (index, g) -> (index, g)) |> Int.Map.of_alist_exn in
    let consumed_gaps = Int.Map.empty in
    let unmoved = [] in
    let rec loop backs gap_map consumed_gaps unmoved =
      match backs with
      | ((i, s, index) as back)::rest ->
        let gap_o = Int.Map.to_alist gap_map |> List.find ~f:(fun (gap_index, g) -> g >= s && gap_index < index) in
        (match gap_o with
        | Some (index, gap) ->
          let rem = gap - s in
          let rem_index = index+s in
          let gap_map = Int.Map.remove gap_map index in
          let gap_map = if rem = 0 then gap_map else (Int.Map.add_exn gap_map ~key:rem_index ~data:rem) in
          let consumed_map = Int.Map.add_exn consumed_gaps ~key:index ~data:(i, s, index) in
          loop rest gap_map consumed_map unmoved
        | None ->
          loop rest gap_map consumed_gaps (back::unmoved))
      | [] -> List.concat [(consumed_gaps |> Int.Map.to_alist |> List.map ~f:snd); unmoved] in
    loop back gap_map consumed_gaps unmoved
    
    
  
  let pack l =
    let lcount = List.sum (module Int) l ~f:(fun (_, a, _, _) -> a) in
    let back = List.rev l |> List.map ~f:(fun (i, s, index, _) -> (i, s, index)) in
    let gaps = l |> List.map ~f:(fun (_, s, index, g) -> (index + s, g)) in
    let nl = fill_gaps back gaps in
    let nlcount = List.sum (module Int) nl ~f:(fun (_, a, _) -> a) in
    assert (lcount = nlcount);
    nl
    

  let rec check_sum (i, n, index) =
    if n = 0 then
      0
    else
      i*index + check_sum(i, n-1, index+1)
    
  let run () =
    let l = input () |> make_intermediate |> pack |> List.sort ~compare:(fun (_, _, i1) (_, _, i2) -> Int.compare i1 i2) in
    l |> List.sum (module Int) ~f:check_sum
end
              
let part1 () =
  Printf.printf "\nPart 1: %d\n" (Part_1.run ())

let part2 () =
  Printf.printf "Part 2: %d\n" (Part_2.run ())  
       
let _ =
  part1 ();
  part2 ()
    
