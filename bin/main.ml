(* Headless tournament driver.  Mirrors `Tournament --bots ... --games N` on the Scala side. *)

open Mancer

let () =
  Printexc.record_backtrace true;
  let usage =
    "main.exe [--games N] [--bots greedy,oracle,lecturer] [--budget MS] [--maxTurns N]\n\
    \          [--out DIR] [--resume DIR] [--seed N] [--special HOUSE] [--mode play|summary] [--dir DIR] [-v]"
  in
  (* Tournament.parse already reads the flags; Arg is only here so unknown
     tokens still produce a usage line. *)
  let argv = Sys.argv in
  (try
     Arg.parse_argv argv
       [
         ("-v", Arg.Unit ignore, " verbose event trace on the first game");
         ("--seed", Arg.Int ignore, "N replay the deals of a previous run");
         ("--games", Arg.Int ignore, "N games per pairing (default 20)");
         ("--bots", Arg.String ignore, "IDS comma-separated (greedy, oracle, lecturer)");
         ("--budget", Arg.Int ignore, "MS search bots' time budget per move (default 1000)");
         ("--maxTurns", Arg.Int ignore, "N (default 300)");
         ("--out", Arg.String ignore, "DIR (default tournament-results/<timestamp>)");
         ("--resume", Arg.String ignore, "DIR continue a previous run");
         ("--special", Arg.String ignore, "HOUSE special house for both players (default Troopers)");
         ("--mode", Arg.String ignore, "play|summary (default play)");
         ("--dir", Arg.String ignore, "DIR summary input");
         ("--include", Arg.String ignore, "specials");
         ("--exclude", Arg.String ignore, "specials");
         ("--minPlays", Arg.Int ignore, "N");
         ("summary", Arg.Unit ignore, " recompute standings from --dir");
       ]
       (fun a -> raise (Arg.Bad ("unexpected argument " ^ a)))
       usage
   with
  | Arg.Help h ->
      print_string h;
      exit 0
  | Arg.Bad h ->
      prerr_string h;
      exit 2);
  Tournament.run (Tournament.parse argv)
