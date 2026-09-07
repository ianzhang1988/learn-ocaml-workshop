open Base
open Scaffold

(* When you start the exercise, the compiler will complain that Frog.create,
 * World.create and create_frog are unused. You can remove this attribute once
 * you get going. *)
[@@@warning "-32"]

module Boarder = struct

  let height = List.length Board.rows
  let width = Board.num_cols

end

module Frog = struct
  type t =
    { position : Position.t;
      img      : Image.t
    } [@@deriving fields]

  let create = Fields.create

  let up    f = {position = { f.position with y = (f.position.y + 1) }; img = Image.Frog_up }
  let down  f = {position = { f.position with y = (f.position.y - 1) }; img = Image.Frog_down }
  let left  f = {position = { f.position with x = (f.position.x - 1) }; img = Image.Frog_left }
  let right f = {position = { f.position with x = (f.position.x + 1) }; img = Image.Frog_right }

  let position t = t.position
  let keep_in_world t =
    let new_pos = 
      Position.{ x = if t.position.x > Boarder.width - 1 then Boarder.width - 1 else if t.position.x < 0 then 0 else t.position.x;
        y = if t.position.y > Boarder.height - 1 then Boarder.height - 1 else if t.position.y <0 then 0 else t.position.y 
      }
    in
    {t with position = new_pos}

end

module Non_frog_character = struct
  module Kind = struct
    type t =
      | Car
      | Log
  end

  type t =
    {
      kind: Kind.t;
      pos:  Position.t;
      speed: int;
    } [@@deriving fields]


  let kind t = t.kind
  let position t = t.pos
  let move t = 
    {t with pos = { t.pos with x = t.pos.x + t.speed }}
  let keep_in_world t =
    let new_pos = 
      { t.pos with x =
          if t.pos.x >= 0 then t.pos.x % Boarder.width 
          else
          Boarder.width - 1
      }
    in
    {t with pos = new_pos}

  (** In units of grid-points/tick. Positive values indicate rightward motion,
     negative values leftward motion. *)
  let horizontal_speed t = t.speed

  let img t =
    match t.kind with
    | Car -> Image.Car1_right
    | Log -> Image.Car1_right
end


module type With_position = sig
  type t
  val keep_in_world: t -> t
end

let keep_in_world (type a) (module M: With_position with type t=a) (entity:a) =
  M.keep_in_world entity

module World = struct
  type t =
    { frog  : Frog.t;
      nfcs   : Non_frog_character.t list
    } [@@deriving fields]

  let create = Fields.create
end

let create_frog () =
  Frog.create ~position:(Position.create ~x:5 ~y:0) ~img:Image.Frog_up
;;

let create () =
  let _presence = List.init 10 ~f:(fun _ -> Random.bool () ) in
  let nfc:Non_frog_character.t = {kind = Car; pos = {x = 0; y = 1}; speed = 1; } in
  World.create ~frog:(create_frog () ) ~nfcs:[nfc]
;;

let tick (w : World.t) =
  (* failwith *)
  (*   "This function will end up getting called every timestep, which happens to \ *)
  (*    be set to 1 second for this game in the scaffold (so you can easily see \ *)
  (*    what's going on). For the first step (just moving the frog/camel around), \ *)
  (*    you can just return [world] here. Later you'll want do interesting things \ *)
  (*    like move all the cars and logs, detect collisions and figure out if the \ *)
  (*    player has died or won. " *)

  let new_nfcs =
    List.map w.nfcs ~f:(
      fun nfc ->
        let nfc_tmp = Non_frog_character.move nfc in
        keep_in_world (module Non_frog_character) nfc_tmp
      )
  in
  {
    w with
    nfcs = new_nfcs
  }
;;

let handle_input (w : World.t) (k : Key.t) =
(*   failwith *)
(*     "This function will end up getting called whenever the player presses one of \ *)
(*      the four arrow keys. What should the new state of the world be? Create and \ *)
(*      return it based on the current state of the world (the [world] argument), \ *)
(*      and the key that was pressed ([key]). Use either [World.create] or the \ *)
(*      record update syntax: *)
(*     { world with frog = Frog.create ... } *)
(* " *)
  let new_frog = 
    let frog_tmp = 
      match k with
      | Arrow_up -> Frog.up w.frog
      | Arrow_down -> Frog.down w.frog
      | Arrow_left -> Frog.left w.frog
      | Arrow_right -> Frog.right w.frog in
    keep_in_world (module Frog) frog_tmp
  in
  (* World.create ~frog:new_frog *)
  { w with frog = new_frog }
;;

let draw (w : World.t) =
  (* failwith *)
  (*   "Return a list with a single item: a tuple consisting of one of the choices \ *)
  (*    in [Images.t] in [scaffold.mli]; and the current position of the [Frog]." *)
  List.concat [
    [(w.frog.img, w.frog.position)];
    List.map w.nfcs ~f:(
        fun nfc -> (Non_frog_character.img nfc, Non_frog_character.position nfc)
    );
  ]
;;

let handle_event (w : World.t) (e : Event.t) =
  (* failwith *)
  (*   "This function should probably be just 3 lines long: [match event with ...]" *)
  match e with
  | Tick -> tick w
  | Keypress k -> handle_input w k
;;

let finished (_ : World.t) =
  (* failwith *)
  (*   "This can probably just return [false] in the beginning." *)
  false
;;
