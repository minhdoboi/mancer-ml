let read_file path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () -> really_input_string ic (in_channel_length ic))

let write_file path text =
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> output_string oc text)

let flag_value argv key =
  let rec go = function a :: v :: _ when a = key -> Some v | _ :: xs -> go xs | [] -> None in
  go (Array.to_list argv)

(* Ties keep the first element. *)
let max_by f = function
  | [] -> invalid_arg "Util.max_by: empty"
  | x :: xs -> List.fold_left (fun a b -> if f b > f a then b else a) x xs

let min_by f = function
  | [] -> invalid_arg "Util.min_by: empty"
  | x :: xs -> List.fold_left (fun a b -> if f b < f a then b else a) x xs
