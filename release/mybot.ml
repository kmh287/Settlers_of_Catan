(* open Definition
open Registry
open Constant
open Util
include Gametype
open BotUtil *)

(** Give your bot a 2-20 character name. *)
let name = "mybot"


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
        let hexIndex = findBestHexWithoutMe game game.gActive in
        let surroundedPlayers = getSurroundedPlayer game hexIndex in
        let player = findMostDangerousPlayer game surroundedPlayers in
        RobberMove(hexIndex, player)
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
        (* 
        => if there is a robber affect myself 
            1. if have a knight card, use it, move it  
            according to robber request  criterion
            2. if don't have a card, buy one if have enough resource
            use it according to the card type.(In this case, only buy
            one time no matter what card we get.)
        => if the dice haven't been rolled, roll the dice
        => try some trade
        => build town ==> build city/road according to 
            some currently unkonw reasons.(Need to figure this out)
        => end turn after all have been done
        *)

        if is_none game.gDiceRolled then Action(RollDice) else Action(EndTurn)
end


(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))
