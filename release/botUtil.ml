open Definition
open Constant
open Util
open Print
open Game
open GameUtil
open GameType

(**********************************************************************)
(******                {Game related functions}                  ******)
(**********************************************************************)
(* change the state to game type *)
let game_of_state = game_of_state



(**********************************************************************)
(******                {Optimization Constants}                  ******)
(**********************************************************************)




(**********************************************************************)
(******              {Initial phase helper functions}            ******)
(**********************************************************************)





(**********************************************************************)
(******              {Build related helper functions}            ******)
(**********************************************************************)
(* return ture if the player has enough resource to build specify
  item, false if don't *)
let hasEnoughResBuild (build:build) (player:gPlayer) : bool =  
  let inv = player.gPInventory in
  match build with
  | BuildRoad _ -> greaterThanEqual inv cCOST_ROAD
  | BuildTown _ -> greaterThanEqual inv cCOST_TOWN
  | BuildCity _ -> greaterThanEqual inv cCOST_CITY
  | BuildCard   -> greaterThanEqual inv cCOST_CARD

(* return index list of all suitable town settlements *)
let allSuitableTowns (game:game) : int list = 
  snd (leftFoldInterList 
    (fun (index,acc) inter -> 
      if(suitableTown game index)
        then (index+1, index::acc)
      else (index+1, acc)) (0, []) game.gInterList
  )

(* if number is 6 or 8 , then 2 point.if 4 or 5 then 1 point,
   else 0 point.  *)
let assignPointOnRollNumber (roll:roll) : int = 
  if (abs(roll - 7) <= 2) 
    then 2 
  else
    if(abs(roll - 7) <= 4) then 1 else 0

(* Give a list of roll number points. 
  Criterion: if number is 6 or 8 and no duplicates, then 3 point. With
  duplicates, then 2 points. Else if 4 or 5 then 1 point, else 0 point. 
  Sum them all together *)
let assignPointOnRoll (rolls : roll list) : int = 
  let assignRoll (roll:roll) : int = 
    if (List.length (List.filter (fun r -> r = roll) rolls) < 2) 
      then 1 + (assignPointOnRollNumber roll)
    else
      assignPointOnRollNumber roll
  in
  List.fold_left (fun sum roll -> sum + (assignRoll roll)) 0 rolls

(* Base points: bricks and lumber are more important, 2 points
  for each, the rest are 1 point each. *)
let assignPointOnTerType (ter : terrain) : int = 
  match resource_of_terrain ter with
    | Some Brick | Some Lumber -> 2
    | Some Wool | Some Ore | Some Grain -> 1
    | _ -> 0

(* Give a list of terrain points. 
  Criterion:  If resources are different from each other, 
  give additional 1 points, besides base points. *)
let assignPointOnTer (ters : terrain list) : int = 
  let basePoints = List.fold_left 
  (fun sum ter -> sum + (assignPointOnTerType ter)) 0 ters in
  let noDupPoints = 
    if (List.for_all 
      (fun t -> (List.length (List.filter (fun ter -> ter = t) ters) < 2) )
      ters)
    then 1 else 0
  in basePoints + noDupPoints


(* assign the current intersection some according to some certain 
  criterion, the higher the point is, the better the location is *)
let assignPoint (game:game) (index:int) : int = 
  let hexList = game.gHexList in
  let adjacentHex = List.fold_left 
    (fun acc hIndex -> (nthOfHexList hexList hIndex)::acc) 
    [] (adjacent_pieces index) in
  let rollList = mapHexList (fun (ter, r) -> r) adjacentHex in
  let terList = mapHexList (fun (ter, r) -> ter) adjacentHex in
  (* points assigned based on the roll number of adjacent hexes *)
  let rollPoint = assignPointOnRoll rollList in
  let terPoint = assignPointOnTer terList in
  rollPoint + terPoint



(* assign points to all the index list, return the index with 
 max point. Pair in the fold represent(index, point) *)
let assignIntersPoints (game:game) (interIndexList:int list) : (int * int) = 
  List.fold_left 
  (fun (maxIndex, max) index -> 
    let curPoint = assignPoint game index in
    if curPoint > max then (index, curPoint) else (maxIndex, max))
  (0, 0) interIndexList

(* return the best location for settle the next town *)
let findBestTownLocation (game:game) : int = 
  let allSuitable = allSuitableTowns game in
  snd(assignIntersPoints game allSuitable)

(* return the best place for building a road *)
let findBestRoadLocation (game:game) : line = 
  failwith "unimplemented findBestRoadLocation"

(**********************************************************************)
(******              {Player related helper functions}           ******)
(**********************************************************************)

(* return a gPlayer list that surround a certain hex *)
let getSurroundedPlayer (game:game) (hexIndex:int) : gPlayer list = 
  let interList = game.gInterList in
  List.fold_left (fun acc iIndex -> 
    match (nthOfInterList interList iIndex) with
    | Some (c, s) -> (findPlayer game c)::acc
    | _ -> acc
  ) [] (piece_corners hexIndex)

(* assign point on a certain hex.
  Criterion: 1. roll number 2. resource type: number of surrounding players*)
let assignPointOnHex (game:game) (hIndex:int) : int = 
  let (ter, roll) = nthOfHexList game.gHexList hIndex in
  let rollPoint = assignPointOnRollNumber roll in
  let terPoint = assignPointOnTerType ter in
  rollPoint + terPoint

(* return the best inter without myself around on the board *)
let findBestHexWithoutMe (game:game) (me:color) : int = 
  let hexList = game.gHexList in
  let hexIndexWithoutMe = snd (leftFoldHexList (fun (index, acc) hex -> 
    if(List.mem (findPlayer game me) 
      (getSurroundedPlayer game index)) then (index+1, acc)
    else (index+1, index::acc)
  ) (0, []) hexList) in
  fst (List.fold_left (fun (maxIndex, max) hIndex -> 
    let curPoint = assignPointOnHex game hIndex in
    if curPoint > max then (hIndex, curPoint) else (maxIndex, max))
  (0, 0) hexIndexWithoutMe)

(* return the player color with greatest potential threateness. 
  return None if there doesn't exist one or (Some color) *)
let findMostDangerousPlayer (game:game) (players:gPlayer list) 
    : color option = 
  if((List.length players) = 0) then None
  else 
    fst (leftFoldPlayerList (fun (c, max) player -> 
      let curVicPoints = checkVictoryPointsPlayer game player in
      if(curVicPoints > max) 
        then (Some (getPlayerColor player), curVicPoints)
      else (c, max) ) (None, -1) players)


(* generate an appropriate robber move *)
let generateRobberMove (game:game) : robbermove = 
  let hexIndex = findBestHexWithoutMe game game.gActive in
  let surroundedPlayers = getSurroundedPlayer game hexIndex in
  let player = findMostDangerousPlayer game surroundedPlayers in
  (hexIndex, player)



(**********************************************************************)
(******              {Action related helper functions}           ******)
(**********************************************************************)
(* return whether there is a robber affecting mine inters *)
let hasRobberOnMine (game:game) (me:gPlayer) : bool = 
  let robberPiece = game.gRobber in
  let affectedPlayers = getSurroundedPlayer game robberPiece in
  memPlayerList me affectedPlayers

(* check wether the player has a knight card in hand *)
let hasKnightCard (player:gPlayer) : bool = 
  memCards Knight player.gPCard

(* return the number of towns of a player *)
let settlementNumber (game:game) (p:color) : int = 
  leftFoldInterList (fun acc inter -> 
    if (inter = Some(p, Town) || inter = Some(p, City)) 
      then (acc+1) else acc) 0 game.gInterList

(* retrun whether the player has bought a card in current turn *)
let hasBoughtCard (game:game) : bool = 
  match game.gCardsBought with
  | Hidden num -> num > 0
  | Reveal cList -> (sizeOfCardList cList) > 0

(* genereate actions after cards play phase in a bot *)
let generateActionAfterCard (game:game) : action = 
  let me = game.gActive in
  if(is_none game.gDiceRolled) then RollDice
  else 
    if((hasEnoughResBuild (BuildTown 0) (findPlayer game me) )
        && (settlementNumber game me < 4))
      then 
        let town = findBestTownLocation game in
        BuyBuild (BuildTown(town))
    else
      if ((hasEnoughResBuild (BuildRoad (me, (0,0))) (findPlayer game me))
          && ((settlementNumber game me) >= 4) )
        then 
          let line = findBestRoadLocation game in
          BuyBuild (BuildRoad(me, line))
      else EndTurn

(* generate appropriate actions *)
(* => if there is a robber affect myself 
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
let generateAction (game:game) : action = 
  let me = findPlayer game game.gActive in
    if(not game.gCardPlayed) 
      then
        if(hasRobberOnMine game me) 
          then
            if(hasKnightCard me) then 
              let robberMove = generateRobberMove game in
              PlayCard (PlayKnight robberMove)
            else 
              if(not (hasBoughtCard game)) then
                if(hasEnoughResBuild BuildCard me) then (BuyBuild BuildCard)
                else generateActionAfterCard game
              else generateActionAfterCard game
        else generateActionAfterCard game
    else generateActionAfterCard game
















