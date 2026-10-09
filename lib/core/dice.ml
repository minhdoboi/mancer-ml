(* 48-bit LCG matching [java.util.Random] / [scala.util.Random], so a seeded
   OCaml game and a Scala replay draw the same integers. *)

type t = { mutable seed : int64 }

let mask = 0xFFFF_FFFF_FFFFL
let multiplier = 0x5DEECE66DL
let addend = 0xBL
let scramble seed = Int64.logand (Int64.logxor seed multiplier) mask
let make seed = { seed = scramble (Int64.of_int seed) }
let make64 seed = { seed = scramble seed }
let copy r = { seed = r.seed }

let make_self () =
  let bits = Int64.of_int (Random.State.bits (Random.State.make_self_init ())) in
  make64 (Int64.logxor bits (Int64.of_float (Unix.gettimeofday () *. 1e6)))

let next r bits =
  r.seed <- Int64.logand (Int64.add (Int64.mul r.seed multiplier) addend) mask;
  Int64.to_int (Int64.shift_right_logical r.seed (48 - bits))

(* java.util.Random.nextInt(bound). The rejection check is 32-bit signed, like Java. *)
let int r n =
  if n <= 0 then invalid_arg "Dice.int";
  let r31 () = next r 31 in
  let m = n - 1 in
  if n land m = 0 then
    Int64.to_int (Int64.shift_right (Int64.mul (Int64.of_int n) (Int64.of_int (r31 ()))) 31)
  else
    let rec loop u =
      let value = u mod n in
      let wrapped = Int32.add (Int32.sub (Int32.of_int u) (Int32.of_int value)) (Int32.of_int m) in
      if Int32.compare wrapped 0l < 0 then loop (r31 ()) else value
    in
    loop (r31 ())

(* scala.util.Random.shuffle: Fisher-Yates, swapping n-1 with nextInt(n) down to 2. *)
let shuffle r xs =
  let a = Array.of_list xs in
  for n = Array.length a downto 2 do
    let k = int r n in
    let tmp = a.(n - 1) in
    a.(n - 1) <- a.(k);
    a.(k) <- tmp
  done;
  Array.to_list a
