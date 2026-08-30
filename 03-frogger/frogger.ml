open Base
open Scaffold

(* When you start the exercise, the compiler will complain that Frog.create,
 * World.create and create_frog are unused. You can remove this attribute once
 * you get going. *)
[@@@warning "-32"]

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

end

let in_canvas_fun x1 x2 y1 y2 =
  fun (pos:Position.t) ->
    let x_in_range = pos.x >= x1 && pos.x < x2 in
    let y_in_range = pos.y >= y1 && pos.y < y2 in
    x_in_range && y_in_range

let in_canvas =
  in_canvas_fun 0 Board.num_cols 0 (List.length Board.rows) 

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
      shadows: bool list;
    } [@@deriving fields]


  let kind t = t.kind
  let position t = t.pos

  (** In units of grid-points/tick. Positive values indicate rightward motion,
     negative values leftward motion. *)
  let horizontal_speed t = t.speed
  let shadows t = t.shadows

  let img t =
    match t.kind with
    | Car -> Image.Car1_right
    | Log -> Image.Car1_right

end

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
  let shadows = List.init 10 ~f:(fun _ -> Random.bool () ) in
  let nfc:Non_frog_character.t = {kind = Car; pos = {x = 0; y = 1}; speed = 1; shadows = shadows} in
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
        {nfc with pos = { nfc.pos with x = (nfc.pos.x + nfc.speed) % Board.num_cols;  }}
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
    if in_canvas frog_tmp.position then
      frog_tmp
    else
      w.frog
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
