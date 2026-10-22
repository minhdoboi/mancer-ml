(* The update check: reading a version and telling which one is newer. *)

open Mancer

let check = Alcotest.(check (option (list int)))
let newer a b = Update.compare_versions a b > 0

let parse () =
  check "tag" (Some [ 0; 9; 5 ]) (Update.parse_version "v0.9.5");
  check "no v" (Some [ 0; 9; 5 ]) (Update.parse_version "0.9.5");
  check "commits after the tag" (Some [ 0; 9; 5 ]) (Update.parse_version "v0.9.5-3-gabc1234");
  check "dirty tree" (Some [ 0; 9; 5 ]) (Update.parse_version "v0.9.5-dirty");
  check "two parts" (Some [ 1; 2 ]) (Update.parse_version "1.2");
  check "one part" None (Update.parse_version "v3");
  check "dev build" None (Update.parse_version "dev");
  check "commit hash" None (Update.parse_version "4ee54d2");
  check "hash with a dot" None (Update.parse_version "4.ee54d2");
  check "empty" None (Update.parse_version "");
  check "spaces" None (Update.parse_version "   ")

let compare () =
  Alcotest.(check bool) "newer" true (newer [ 0; 9; 6 ] [ 0; 9; 5 ]);
  Alcotest.(check bool) "older" false (newer [ 0; 9; 5 ] [ 0; 9; 6 ]);
  Alcotest.(check bool) "same" false (newer [ 0; 9; 5 ] [ 0; 9; 5 ]);
  Alcotest.(check bool) "missing parts are zero" false (newer [ 0; 9 ] [ 0; 9; 0 ]);
  Alcotest.(check bool) "missing parts are zero, other way" true (newer [ 0; 9; 1 ] [ 0; 9 ]);
  Alcotest.(check bool) "major" true (newer [ 1; 0; 0 ] [ 0; 9; 9 ]);
  Alcotest.(check bool) "minor" true (newer [ 0; 10; 0 ] [ 0; 9; 9 ])

let () =
  Alcotest.run "update"
    [
      ("version", [ Alcotest.test_case "parses tags" `Quick parse ]);
      ("compare", [ Alcotest.test_case "orders versions" `Quick compare ]);
    ]
