open Core

let input_file = "../input/day07/input.txt"


let _peek a ~f = begin
    f a;
    a
  end
  
module Part_1 = struct
  let cards = ['A'; 'K'; 'Q'; 'J'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2']
  let card_scores = cards |> List.rev |> List.mapi ~f:(fun i c -> (c, i + 1)) |> Char.Map.of_alist_exn 

  let input () =
    In_channel.read_lines input_file
    |> List.map ~f:(fun line ->
           let [@warning "-8"] [hand_s; bit_s] = String.split line ~on:' ' in
           (hand_s, String.to_list hand_s |> List.map ~f:(Char.Map.find_exn card_scores), Int.of_string bit_s)
         )
    
  let score hand =
    let counts = hand |> List.sort ~compare:Int.compare |> List.group ~break:(fun a b -> a <> b) |> List.map ~f:List.length |> List.sort ~compare:Int.compare in
    let type_score = match counts with
      | [5] -> 7
      | [1;4] -> 6
      | [2;3] -> 5
      | [1;1;3] -> 4
      | [1;2;2] -> 3
      | [1;1;1;2] -> 2
      | [1;1;1;1;1] -> 1
      | _ -> failwith (Printf.sprintf "Invalid counts %s" (counts |> List.map ~f:Int.to_string |> String.concat ~sep:", ")) in
    hand
    |> List.fold ~init:0 ~f:(fun acc card -> type_score * Int.pow 13 6 + 13 * acc + card)

  let run () =
    let games = input () in
    games
    |> List.map ~f:(fun (hand_s, hand, bid) -> (hand_s, score hand, bid))
    |> List.sort ~compare:(fun (_, a, _) (_, b, _) -> Int.compare a b)
    |> List.mapi ~f:(fun i (_, _, bid) -> ((i+1) * bid))
    |> List.sum (module Int) ~f:Fun.id
end

module Part_2 = struct
  let cards = ['A'; 'K'; 'Q'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'; 'J']
  let card_scores = cards |> List.rev |> List.mapi ~f:(fun i c -> (c, i + 1)) |> Char.Map.of_alist_exn 

 let input () =
    In_channel.read_lines input_file
    |> List.map ~f:(fun line ->
           let [@warning "-8"] [hand_s; bit_s] = String.split line ~on:' ' in
           (hand_s,  Int.of_string bit_s)
         )

 let score hand_s =
   let hand = String.to_list hand_s |> List.map ~f:(Char.Map.find_exn card_scores) in
   let jack_count: int = String.count hand_s ~f:Char.(fun c -> c = 'J') in
   let type_score_cards = String.filter hand_s ~f:Char.(fun c -> c <> 'J') in
   let counts: int list =
     type_score_cards
     |> String.to_list
     |> List.sort ~compare:Char.compare
     |> List.group ~break:Char.(fun a b -> a <> b)
     |> List.map ~f:List.length
     |> List.sort ~compare:Int.compare
     |> List.rev in
   let counts_with_jack =
     match counts with
     | [] -> [jack_count]
     | x::rest -> (x + jack_count)::rest in
   let type_score = match  counts_with_jack |> List.rev with
     | [5] -> 7
     | [1;4] -> 6
     | [2;3] -> 5
     | [1;1;3] -> 4
     | [1;2;2] -> 3
     | [1;1;1;2] -> 2
     | [1;1;1;1;1] -> 1
     | _ -> failwith (Printf.sprintf "Invalid counts %s" (counts_with_jack |> List.map ~f:Int.to_string |> String.concat ~sep:", ")) in
   hand
   |> List.fold ~init:0 ~f:(fun acc card -> type_score * Int.pow 13 6 + 13 * acc + card)
    
 let run () =
   let games = input () in
   games
   |> List.map ~f:(fun (hand_s, bid) -> (hand_s, score hand_s, bid))
   |> List.sort ~compare:(fun (_, a, _) (_, b, _) -> Int.compare a b)
   |> List.mapi ~f:(fun i (_, _, bid) -> ((i+1) * bid))
   |> List.sum (module Int) ~f:Fun.id
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
