(** Small helpers shared by the library, the bots and the executables. *)

val read_file : string -> string
(** Whole file, in binary mode. Raises [Sys_error] if it cannot be read. *)

val write_file : string -> string -> unit
(** [write_file path text] overwrites [path]; the directory must exist. *)

val flag_value : string array -> string -> string option
(** [flag_value argv "--out"]: the argument right after the flag, if any. *)

val max_by : ('a -> 'b) -> 'a list -> 'a
(** Element with the greatest key; the first one on ties. Raises [Invalid_argument] on [[]]. *)

val min_by : ('a -> 'b) -> 'a list -> 'a
(** Element with the least key; the first one on ties. Raises [Invalid_argument] on [[]]. *)
