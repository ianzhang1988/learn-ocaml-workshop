open Base
open Scaffold

(* When you start the exercise, the compiler will complain that Frog.create,
 * World.create and create_frog are unused. You can remove this attribute once
 * you get going. *)
[@@@warning "-32"]

module Frog = struct
  type t =
    { position : Position.t
    } [@@deriving fields]

  let create = Fields.create

  let up    f = {position = { f.position with y = (f.position.y + 1) } }
  let down  f = {position = { f.position with y = (f.position.y - 1) } }
  let left  f = {position = { f.position with x = (f.position.x - 1) } }
  let right f = {position = { f.position with x = (f.position.x + 1) } }

end

module World = struct
  type t =
    { frog  : Frog.t
    } [@@deriving fields]

  let create = Fields.create
end

let create_frog () =
  Frog.create ~position:(Position.create ~x:5 ~y:1)
;;

let create () =
  World.create ~frog:(create_frog () )
;;

let tick (w : World.t) =
  (* failwith *)
  (*   "This function will end up getting called every timestep, which happens to \ *)
  (*    be set to 1 second for this game in the scaffold (so you can easily see \ *)
  (*    what's going on). For the first step (just moving the frog/camel around), \ *)
  (*    you can just return [world] here. Later you'll want do interesting things \ *)
  (*    like move all the cars and logs, detect collisions and figure out if the \ *)
  (*    player has died or won. " *)
  w
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
  match k with
  | Arrow_up -> Frog.up w.frog
  | Arrow_down -> w.frog
  | Arrow_left -> w.frog
  | Arrow_right -> w.frog in
  World.create ~frog:new_frog
;;

let draw (w : World.t) =
  (* failwith *)
  (*   "Return a list with a single item: a tuple consisting of one of the choices \ *)
  (*    in [Images.t] in [scaffold.mli]; and the current position of the [Frog]." *)
  [(Image.Frog_up, w.frog.position)]
;;

let handle_event (w : World.t) (e : Event.t) =
  (* failwith *)
  (*   "This function should probably be just 3 lines long: [match event with ...]" *)
  match e with
  | Tick -> w
  | Keypress k -> handle_input w k
;;

let finished (_ : World.t) =
  (* failwith *)
  (*   "This can probably just return [false] in the beginning." *)
  false
;;
