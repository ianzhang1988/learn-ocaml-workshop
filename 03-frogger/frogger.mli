open Scaffold

module Non_frog_character : sig
  module Kind : sig
    type t =
      | Car
      | Log
  end

  type t

  val kind : t -> Kind.t
  val position : t -> Position.t

  (** In units of grid-points/tick. Positive values indicate rightward motion,
     negative values leftward motion. *)
  val horizontal_speed : t -> int
end

module World : sig
  type t
end


val create : unit -> World.t
val tick : World.t -> World.t
val handle_input : World.t -> Key.t -> World.t
val handle_event : World.t -> Event.t -> World.t
val draw : World.t -> Display_list.t
val finished : World.t -> bool
