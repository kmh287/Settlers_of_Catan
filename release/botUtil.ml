(* open GameType
open Model
open GameUtil
open Game *)

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



(**********************************************************************)
(******              {Player related helper functions}           ******)
(**********************************************************************)
(* return the victory points of a player *)
let getVicPoints (game:game) (player:gPlayer) : int = 
  failwith "unimplemented"

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
      let curVicPoints = getVicPoints game player in
      if(curVicPoints > max) 
        then (Some (getPlayerColor player), curVicPoints)
      else (c, max) ) (None, -1) players)






















