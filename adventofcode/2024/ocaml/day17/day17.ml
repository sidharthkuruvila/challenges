open Core

let input_file = "../input/day17/input.txt"


module Part_1 = struct
 
  type env = {a: int; b: int; c: int; pc: int; out: int list } [@@deriving show]
  let input () =
    let sc = Scanf.Scanning.from_file input_file in
    Scanf.bscanf sc "Register A: %d\nRegister B: %d\nRegister C: %d\n\nProgram: %s"
      (fun a b c ps -> (a, b, c, String.split ps ~on:',' |> List.map ~f:Int.of_string |> List.to_array))


    let get {a; b; c; _} o =
      match o with
      | 0 | 1 | 2 | 3 -> o
      | 4 -> a
      | 5 -> b
      | 6 -> c
      | _ -> failwith "Unexpected operand" 
    let get_a e = e.a 
    let get_b e = e.b 
    let get_c e = e.c 
    let set_a e v = {e with a = v } 
    let set_b e v = {e with b = v} 
    let set_c e v = {e with c = v} 
    let write e v = { e with out =  v mod 8 :: e.out}
                    
    let inst_names = [|
      "adv"; "bxl"; "bst"; "jnz";"bxc";"out";"bdv";"cdv" |]
   let insts = [|
      (fun e o -> (* adv *)
         let a = get_a e in
         set_a e (a / (1 lsl (get e o))));
      (fun e o -> (* bxl *)
         let b = get_b e in
         set_b e (Int.(b lxor o)));
      (fun e o -> (* bst *)
         set_b e ((get e o) mod 8));
      (fun e o -> (* jnz *)
         let a = get_a e in
         if a = 0 then e
         else {e with pc = o - 2});
      (fun e _ -> (* bxc *)
         let b = get_b e in
         let c = get_c e in
         let v = Int.(b lxor c) in
         {e with b = v});
      (fun e o -> (* out *)
         write e (get e o));
      (fun e o -> (* bdv *)
         let a = get_a e in
         set_b e (a / (1 lsl (get e o))));
      (fun e o -> (* cdv *)
         let a = get_a e in
         set_c e (a / (1 lsl (get e o))));    
    |]

   let debug_inst e pa =
     if e.pc >= Array.length pa then
       Printf.printf "Done!\n%!"
     else
       Printf.printf "%s %d -> %d%!\n" inst_names.(pa.(e.pc)) pa.(e.pc + 1) (get e  pa.(e.pc + 1))

   
  let run () =
    Printf.printf "\n";
    let (a, b, c, pa) = input () in
    let e = {a; b; c; pc = 0; out = []} in
    let rec loop e  =
      Printf.printf "env = %s\n\n%!" (show_env e);
      debug_inst e pa;

      if e.pc >= Array.length pa then
        List.rev e.out
      else
        let operator = pa.(e.pc) in
        let operand = pa.(e.pc+1) in
        let ne = insts.(operator) e operand in
        loop {ne with pc = ne.pc + 2} in
    loop e |> List.map ~f:Int.to_string |> String.concat ~sep:","
end


module Part_2 = struct
  type env = {a: int; b: int; c: int; pc: int; out: int list } [@@deriving show]
  
  let inst_names = [|
    "adv"; "bxl"; "bst"; "jnz";"bxc";"out";"bdv";"cdv" |]

  let disassemble (a, b, c, pa) =
    Printf.printf "A: %d, B: %d, C: %d\n" a b c;
    for i = 0 to (Array.length pa) / 2 - 1 do
      let instr = pa.(i*2) in
      let oper = pa.(i*2 + 1) in
      let oper_s = if 0 <= oper && oper <= 3 then Int.to_string oper
        else if oper = 4 then "A"
        else if oper = 5 then "B"
        else if oper = 6 then "C"
        else failwith "Unsupported operand" in
      Printf.printf "%2d %s %s\n" (i*2)  inst_names.(instr)  oper_s
    done
  
    let get {a; b; c; _} o =
      match o with
      | 0 | 1 | 2 | 3 -> o
      | 4 -> a
      | 5 -> b
      | 6 -> c
      | _ -> failwith "Unexpected operand" 
    let get_a e = e.a 
    let get_b e = e.b 
    let get_c e = e.c 
    let set_a e v = {e with a = v } 
    let set_b e v = {e with b = v} 
    let set_c e v = {e with c = v} 
    let write e v = { e with out =  v mod 8 :: e.out}
                    
    let inst_names = [|
      "adv"; "bxl"; "bst"; "jnz";"bxc";"out";"bdv";"cdv" |]
   let insts = [|
      (fun e o -> (* adv *)
         let a = get_a e in
         set_a e (a / (1 lsl (get e o))));
      (fun e o -> (* bxl *)
         let b = get_b e in
         set_b e (Int.(b lxor o)));
      (fun e o -> (* bst *)
         set_b e ((get e o) mod 8));
      (fun e o -> (* jnz *)
         let a = get_a e in
         if a = 0 then e
         else {e with pc = o - 2});
      (fun e _ -> (* bxc *)
         let b = get_b e in
         let c = get_c e in
         let v = Int.(b lxor c) in
         {e with b = v});
      (fun e o -> (* out *)
         write e (get e o));
      (fun e o -> (* bdv *)
         let a = get_a e in
         set_b e (a / (1 lsl (get e o))));
      (fun e o -> (* cdv *)
         let a = get_a e in
         set_c e (a / (1 lsl (get e o))));    
    |]


  let input () =
    let sc = Scanf.Scanning.from_file input_file in
    Scanf.bscanf sc "Register A: %d\nRegister B: %d\nRegister C: %d\n\nProgram: %s"
      (fun a b c ps -> (a, b, c, String.split ps ~on:',' |> List.map ~f:Int.of_string |> List.to_array))
      
  let run () =
    let (a, b, c, pa) = input () in
    disassemble (a, b, c, pa);
    ""
end


(* 0 bst A copy the lowest 3 bits of A to B
 2 bxl 3 Flip the three bits in b
 4 cdv B Left shive a b times and write to C
   6 adv 3 bit shift A right 3 times
 8 bxc 3 B - B xor C
   10 bxl B B = B xor 
   
12 out B
   14 jnz 0*)
              
let part1 () =
  Printf.printf "\nPart 1: %s\n" (Part_1.run ())

let part2 () =
  Printf.printf "Part 2: %s\n" (Part_2.run ())
       
let _ =
  part1 ();
  part2 ()  
