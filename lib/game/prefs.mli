(** User preferences in the [.mancer] folder, ported from Scala [Storage].

    The desktop Scala game keeps a Java properties file at [<home>/.mancer/user.prefs]. Key names and defaults
    here match [Storage.scala] so both front ends can share the file; {!persist} writes it straight away, like
    [Storage.persist]. Keys under [setup.*] are extra: the OCaml setup screens (seats, single specials, bot
    budget, portraits). Override the file location with [$MANCER_USER_PREFS] (used by tests). *)

type t
(** An open preferences file: its path plus the live key/value table. *)

val key_user_name : string
val key_card_theme : string
val key_server : string
val key_server_port : string

type side = Player | Ai  (** Whose house pool a [class.choice] key holds. *)

val key_class_choice : side -> string
val key_ai_opponent : string
val key_player_portrait : string
val key_sound_level : string
val key_campaign_completed : string
val key_campaign_position : string
val key_campaign_night_rest : string
val key_campaign_wake_far : string
val key_campaign_class : string
val key_debug_mode : string
val key_ambient_sound : string
val key_language : string
val key_setup_seats : string
val key_setup_specials : string
val key_setup_budget : string
val key_setup_portraits : string

val default_path : unit -> string
(** [$MANCER_USER_PREFS], else [<home>/.mancer/user.prefs]. *)

val load : ?path:string -> unit -> t
(** Reads the file when it exists, else starts empty. Never raises. *)

val path : t -> string
val get : t -> string -> string option

val set : t -> string -> string -> unit
(** [set t k v] stores one property and writes the file. *)

val persist : t -> (string * string) list -> unit
(** [persist t kvs] updates the properties and stores the file, like [Storage.persist]. Never raises (a
    missing home just skips the write). *)

val save : t -> unit

val language : t -> string
(** [language] key, else ["en"]. *)

val set_language : t -> string -> unit

val saved_language : unit -> string option
(** The [language] key from the default file, without opening a {!t}: what [Assets.lang] falls back to when
    [$MANCER_LANG] is unset. Never raises. *)

val sound_level : t -> float
(** [sound.level], clamped to [0..1], else [1]. *)

val set_sound_level : t -> float -> unit
val player_portrait : t -> string
val set_player_portrait : t -> string -> unit
val ai_opponent : t -> string
val set_ai_opponent : t -> string -> unit
val debug_mode : t -> bool
val set_debug_mode : t -> bool -> unit

val ambient_sound : t -> bool
(** Lobby ambient sound; on by default. *)

val set_ambient_sound : t -> bool -> unit
val user_name : t -> string option
val card_theme : t -> string
val class_choice : t -> side -> string list
val set_class_choice : t -> side -> string list -> unit
val campaign_completed : t -> string list
val campaign_position : t -> string
val campaign_night_rest : t -> bool
val campaign_wake_far : t -> bool

val complete_campaign : t -> ?extras:string list -> string -> unit
(** Adds [id] and [extras] to completed stops and moves the token to {!Campaign.next_position} of [id], like
    [Storage.completeCampaign]. *)

val reset_campaign : t -> unit
val reset_night_campaign : t -> unit
val mark_night_rested : t -> unit
val mark_wake_far : t -> unit

val default_campaign_class : string
(** The house key the player starts a campaign with when nothing was chosen yet. *)

val campaign_class : t -> string
(** The stored campaign class: the special house the player plays with all through the campaign. *)

val set_campaign_class : t -> string -> unit
(** Stores the campaign class. It outlives a campaign restart. *)

val setup_seats : t -> string * string
(** [setup.seats] as [(p1, p2)] seat names, else [("human", "oracle")]. *)

val set_setup_seats : t -> string * string -> unit
val setup_specials : t -> string list * string list
val set_setup_specials : t -> string list * string list -> unit
val setup_budget : t -> int
val set_setup_budget : t -> int -> unit

val setup_portraits : t -> string * string
(** [setup.portraits], falling back to [player.portrait] for player one. *)

val set_setup_portraits : t -> string * string -> unit
(** Also updates [player.portrait] so the Scala lobby shows the same face. *)

val parse_seat :
  allowed:string list -> fallback:[ `Human | `Bot of string ] -> string -> [ `Human | `Bot of string ]
(** A persisted seat name back to a seat; unknown bot ids give [fallback]. *)
