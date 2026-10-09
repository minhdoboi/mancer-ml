(* Asset text helpers. *)

open Mancer

let check = Alcotest.(check string)

let format () =
  check "plain" "a 3 b 4" (Assets.format "a {0} b {1}" [ "3"; "4" ]);
  check "two digits" "x" (Assets.format "{10}" (List.init 11 (fun i -> if i = 10 then "x" else "-")));
  check "unknown index stays" "{2} and 5" (Assets.format "{2} and {0}" [ "5" ]);
  check "literal braces stay" "{ x } {" (Assets.format "{ x } {" [ "1" ]);
  check "empty braces stay" "{}" (Assets.format "{}" [ "1" ]);
  check "arguments are not re-substituted" "{1} B" (Assets.format "{0} {1}" [ "{1}"; "B" ])

let () = Alcotest.run "assets" [ ("format", [ Alcotest.test_case "placeholders" `Quick format ]) ]
