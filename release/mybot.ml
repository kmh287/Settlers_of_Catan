open Definition
open Registry
open Constant
open Util
open GameType
open BotUtil

(** Give your bot a 2-20 character name. *)
let name = "mybot"

(* type weights = {
mutable turnNum         : int; 
mutable brickPoints     : int; 
mutable woolPoints      : int;
mutable orePoints       : int; 
mutable grainPoints     : int;
mutable lumberPoints    : int; 
mutable noDupPoints     : int;   
}

let weightRecord = {
  turnNum       = 0;
  brickPoints   = 2;
  woolPoints    = 1;
  orePoints     = 1;
  grainPoints   = 1;
  lumberPoints  = 2;
  } *)

module Bot = functor (S : Soul) -> struct
  (* If you use side effects, start/reset your bot for a new game *)
  let initialize () = ()

  (* Invalid moves are overridden in game *)
  let handle_request (s : state) : move =
    let game = game_of_state s in
    match game.gNextRequest with
      | InitialRequest -> 
         (* find the best town location and choose one point from
         its adjacent points as the end of the road *)
        let townLoc = findBestTownLocation game in
        InitialMove(townLoc, List.hd (adjacent_points townLoc ))
      | RobberRequest -> 
          RobberMove(generateRobberMove game)
      | DiscardRequest-> DiscardMove(0,0,0,0,0)
      | TradeRequest -> 
        (* reject trade request from most dangerous player, simply 
        accept all the oter trade request. Haven't thought about 
        unfair trade yet *)
        let mostDangerousPlayer = 
          findMostDangerousPlayer game game.gPlayerList in
        if(Some game.gActive = mostDangerousPlayer) 
          then TradeResponse(false)
        else TradeResponse(true)
      | ActionRequest -> 
          Action(generateAction game)
end


(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))




  