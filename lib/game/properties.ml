(* Java [.properties] files: the format of [user.prefs] and the i18n tables. *)

(* [Properties.load]: [\uXXXX], [\n \r \t], and a backslash before anything
   else quotes that character. *)
let unescape s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i >= n then ()
    else if s.[i] = '\\' && i + 1 < n then (
      match s.[i + 1] with
      | 'u' when i + 5 < n -> (
          match int_of_string_opt ("0x" ^ String.sub s (i + 2) 4) with
          | Some cp when Uchar.is_valid cp ->
              Buffer.add_utf_8_uchar b (Uchar.of_int cp);
              go (i + 6)
          | _ -> go (i + 2))
      | 'n' ->
          Buffer.add_char b '\n';
          go (i + 2)
      | 'r' ->
          Buffer.add_char b '\r';
          go (i + 2)
      | 't' ->
          Buffer.add_char b '\t';
          go (i + 2)
      | '\n' ->
          (* line continuation: skip the newline and leading whitespace *)
          let j = ref (i + 2) in
          while !j < n && (s.[!j] = ' ' || s.[!j] = '\t') do
            incr j
          done;
          go !j
      | c ->
          Buffer.add_char b c;
          go (i + 2))
    else (
      Buffer.add_char b s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents b

(* [Properties.store] escapes backslashes, control characters, non-Latin1 as
   [\uXXXX], and the [: = # !] syntax characters in keys. *)
let escape s ~is_key =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | ' ' when is_key -> Buffer.add_string b "\\ "
      | '=' when is_key -> Buffer.add_string b "\\="
      | ':' when is_key -> Buffer.add_string b "\\:"
      | '#' when is_key -> Buffer.add_string b "\\#"
      | '!' when is_key -> Buffer.add_string b "\\!"
      | c when Char.code c < 0x20 || Char.code c = 0x7f ->
          Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

(* First unescaped [= : <space>] ends the key, like [Properties.load]. *)
let split_line line =
  let n = String.length line in
  let rec find i =
    if i >= n then None
    else if line.[i] = '\\' then find (i + 2)
    else if line.[i] = '=' || line.[i] = ':' then Some (i, i + 1)
    else if line.[i] = ' ' || line.[i] = '\t' then (
      let j = ref (i + 1) in
      while !j < n && (line.[!j] = ' ' || line.[!j] = '\t') do
        incr j
      done;
      if !j < n && (line.[!j] = '=' || line.[!j] = ':') then (
        incr j;
        Some (i, !j))
      else Some (i, !j))
    else find (i + 1)
  in
  match find 0 with
  | None -> (unescape (String.trim line), "")
  | Some (ki, vi) ->
      (unescape (String.trim (String.sub line 0 ki)), unescape (String.trim (String.sub line vi (n - vi))))

let trailing_backslashes s =
  let rec go i n = if i >= 0 && s.[i] = '\\' then go (i - 1) (n + 1) else n in
  go (String.length s - 1) 0

let lstrip s =
  let n = String.length s in
  let i = ref 0 in
  while !i < n && (s.[!i] = ' ' || s.[!i] = '\t') do
    incr i
  done;
  String.sub s !i (n - !i)

let load_into tbl file =
  try
    In_channel.with_open_text file (fun ic ->
        let add line =
          let k, v = split_line line in
          Hashtbl.replace tbl k v
        in
        (* [continued]: [buf] holds the start of a logical line that the previous physical line left open. *)
        let rec loop ~continued buf =
          match In_channel.input_line ic with
          | None -> if continued then add buf
          | Some raw ->
              let raw = if continued then lstrip raw else raw in
              let is_comment =
                (not continued) && match lstrip raw with "" -> true | t -> t.[0] = '#' || t.[0] = '!'
              in
              if is_comment then loop ~continued:false ""
              else if trailing_backslashes raw land 1 = 1 then
                (* an odd trailing run of backslashes continues the line *)
                loop ~continued:true (buf ^ String.sub raw 0 (String.length raw - 1))
              else (
                add (buf ^ raw);
                loop ~continued:false "")
        in
        loop ~continued:false "")
  with Sys_error _ -> ()

let load file =
  let tbl = Hashtbl.create 64 in
  if Sys.file_exists file then load_into tbl file;
  tbl
