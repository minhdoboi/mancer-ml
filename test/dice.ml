(* java.util.Random / scala.util.Random sequence. *)

open Mancer

let ints seed xs =
  let r = Dice.make seed in
  List.map (fun bound -> Dice.int r bound) xs

let () =
  let open Alcotest in
  run "dice"
    [
      ( "java.util.Random",
        [
          test_case "seed 1 nextInt bounds" `Quick (fun () ->
              check (list int) "seq" [ 1; 1; 1; 3; 2; 6; 2; 6; 78 ] (ints 1 [ 2; 3; 4; 5; 6; 7; 8; 10; 100 ]));
          test_case "seed 1 nextInt(6)" `Quick (fun () ->
              check (list int) "d6" [ 3; 4; 1; 3; 2; 4; 2; 4; 4; 4 ] (ints 1 (List.init 10 (fun _ -> 6))));
          test_case "seed 0 nextInt(5)" `Quick (fun () ->
              check (list int) "d5" [ 0; 3; 4; 2; 0; 3; 1; 1 ] (ints 0 (List.init 8 (fun _ -> 5))));
          test_case "seed 42 shuffle" `Quick (fun () ->
              check (list int) "perm" [ 1; 0; 4; 5; 3; 2 ] (Dice.shuffle (Dice.make 42) [ 0; 1; 2; 3; 4; 5 ]));
        ] );
    ]
