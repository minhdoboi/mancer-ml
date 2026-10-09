(* Special house Dudes — port of priv/sp/house/DudeMancer.scala. Plain creatures, no effects. *)

open Sp
open Common

let dude name a life = creature ~key:name ~attack:(attack a) ~life ~description:"" ()

let cards =
  [
    dude "Bred" 4 17;
    dude "Two P" 4 27;
    dude "Axl" 5 26;
    dude "Bill Bull" 7 31;
    dude "El Gado" 8 46;
    dude "Andore" 9 52;
    dude "Rolento" 10 65;
    dude "Edi E" 10 99;
  ]

let house = special_house ~key:"Dudes" ~id:13 cards
