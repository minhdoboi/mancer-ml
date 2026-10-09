(** Java [.properties] files, the format of [user.prefs] and the i18n tables.

    Reading follows [Properties.load]: blank lines and lines starting with [#] or [!] are comments, the key
    ends at the first unescaped [=], [:] or space, an odd run of trailing backslashes continues the line (the
    next line loses its leading whitespace), and [\uXXXX], [\n], [\r], [\t] are decoded. *)

val load_into : (string, string) Hashtbl.t -> string -> unit
(** Adds the file's entries, later ones replacing earlier ones. A file that cannot be read adds nothing. *)

val load : string -> (string, string) Hashtbl.t
(** A fresh table from the file; empty when it does not exist or cannot be read. *)

val escape : string -> is_key:bool -> string
(** [Properties.store] encoding of a key or value. *)
