(** Card text and art for front ends, from the mancer-assets checkout.

    The checkout is [$MANCER_ASSETS], else [assets/] (a git submodule); [bin/sync_assets.ml] builds it from
    the Scala repo. Without it, text falls back to the i18n keys ([fire.wall]) and art to [None]. *)

open Sp

val root : unit -> string

val load_properties : string -> (string, string) Hashtbl.t
(** A Java [.properties] file as key → value, [\uXXXX] escapes decoded; empty if the file is missing. *)

val lang : unit -> string
(** [$MANCER_LANG] when set, else the [language] key from [<home>/.mancer/user.prefs] (see {!Prefs}), else
    [en]. A language change takes effect on restart, like the Scala settings dialog. *)

val text : string -> string
(** The string for an i18n key from [i18n/<lang>.properties], else the English one, else the key itself.
    Control characters become spaces. *)

val has_key : string -> bool
(** True when the key exists in the chosen or English bundle. *)

val format : string -> string list -> string
(** Replaces [{0}], [{1}], … in an i18n template (Java [MessageFormat] placeholders). *)

val campaign : string -> string option
(** Image [campaign/<name>.jpg|png] (map, epilogue). *)

val clean : string -> string
(** Control characters to spaces; terminal and font renderers choke on them. *)

val card_name : card -> string

val english_card_name : card -> string
(** The card's English name, whatever the UI language. Sound clips are keyed by it. *)

val describe : ?state:game_state -> ?player:player_id -> card -> string
(** Card text. When [state] and [player] are given, [{0}] placeholders (Lightning damage, …) are filled from
    the board, as in Scala [card.description]. *)

val basic_houses : game_state -> player_id -> (string * string) array
(** The four basic houses of this player's deck, as (name, power noun): the classic four in classic mode,
    the standard four in standard mode. The power noun is the [<house>.mana] string of the language, or
    the house name when the language does not need another form. *)

val basic_house_names : game_state -> player_id -> string array
(** The names of the four basic houses, as this player's deck names them. *)

val fill_basic_houses : string -> (string * string) array -> string
(** Replaces the [@0] … [@3] markers in a card text with the basic house names, and the [#0] … [#3]
    markers with the nouns their power takes, in slot order. *)

val card_art : card -> string option
(** [cards/<key>.jpg|png], else the English card name; creatures are 80x80, spells 95x103. *)

val background : house -> string option
(** [backgrounds/<house key>], else the English house name (["dark priest"]), else [backgrounds/default]
    (832x800). *)

val background_layer : house -> string -> string option
(** A layer of a layered backdrop: [background_layer house "-sky"] and ["-ground"]. The ground is the flat
    picture with its sky cut out (a PNG with alpha), the sky the drifting panorama behind it. *)

val creature_frame : unit -> string option
(** Stone creature frame ([raka], 90x102). *)

val spell_frame : unit -> string option
(** Stone spell frame ([rakaSpell], 94x104). *)

val slot_frame : unit -> string option
(** Empty board-slot frame ([slot]). *)

val combat : string -> string option
(** Status overlay ([sand], [crystalize], [shield], …). *)

val ui : string -> string option
(** Command-bar icon [ui/<name>.png] (quit, settings, play, …). *)

val portrait : string -> string option
(** Face [portraits/<id>.png]. *)

val particle : string -> string option
(** Particle sprite ([smoke], …). *)

val shader_pair : string -> (string * string) option
(** Vertex and fragment paths [shaders/<name>.vs|.fs], or [None] if either is missing. *)

val sound : string -> string option
(** Clip for a Scala [sounds] key ([move], [meditate], …) or a card sound ([flamewave]). *)
