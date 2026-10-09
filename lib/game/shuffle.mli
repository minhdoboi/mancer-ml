(** Deck and opening-mana builder ([CardShuffle.scala]).

    Each player gets four cards from every basic house and four from the special, under the same membership /
    sum / ban rules as the Choco model. Opening mana for the four basics sums to 19 (first player) or 18
    (second); the special always starts at 2.

    Every house must already have been through [Game.init_cards], which is what fills in [cost] and
    [card_index]. Card indexes are the builder's only notion of a card's identity, so an uninitialised house
    reports the same index for all of its cards and no legal deck exists.

    All randomness comes from [rng] (default: a freshly self-initialised state), so a deal is reproducible
    from its seed. *)

open Sp

type deal = { desc : player_desc; mana : int array }
(** [mana] has five entries (fire, water, air, earth, special). *)

val mana_gens : (int * int) list
(** [(house index, cost)]: [(fire, 3); (water, 5); (earth, 5)]. Exactly one of these cards is in a legal deck.
*)

val create_player :
  ?rng:Random.State.t -> ?exclude:player_desc -> first:bool -> special:house -> house list -> deal
(** The house list must be [[fire; water; air; earth]], in that order; raises [Invalid_argument] otherwise.
    [first] draws the 19-mana opening hand rather than the 18-mana one. [exclude] is the opponent's already
    built deck: shared houses may not reuse those 1-based card indexes. *)

val two_players :
  ?rng:Random.State.t -> ?starting:player_id -> specials:house * house -> house list -> deal * deal
(** Owner is dealt first, then the opponent with exclusion. [starting] names the player who draws the 19-mana
    opening hand; it must be [owner] or [opponent]. Defaults to [owner]. For a given [rng] state the decks do
    not depend on [starting]; only the opening mana does. *)
