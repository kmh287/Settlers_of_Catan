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

(* return a gPlayer list that surround a certain hex *)
let getSurroundedPlayer (game:game) (hexIndex:int) : gPlayer list = 
  let interList = game.gInterList in
  List.fold_left (fun acc iIndex -> 
    match (nthOfInterList interList iIndex) with
    | Some (c, s) -> (findPlayer game c)::acc
    | _ -> acc
  ) [] (piece_corners hexIndex)

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

(* return the delta (only inadqueate ones) of resource needed 
  to build something *)
let deltaResourceNeed (player:gPlayer) (wantBuild:build) : cost = 
  let inv = player.gPInventory in
  let delta = map_cost2 (fun x y -> if(x < y) then (y - x) else 0) in
  match wantBuild with
  | BuildRoad _ -> delta inv cCOST_ROAD
  | BuildTown _ -> delta inv cCOST_TOWN
  | BuildCity _ -> delta inv cCOST_CITY
  | BuildCard   -> delta inv cCOST_CARD  

(* return index list of all suitable town settlements *)
let allSuitableTowns (game:game) : int list = 
  snd (leftFoldInterList 
    (fun (index,acc) inter -> 
      if(suitableTown game index)
        then (index+1, index::acc)
      else (index+1, acc)) (0, []) game.gInterList
  )

(* return a lists contains all type of resources I already have.
  One for each hex, which means if I have two woods, then there 
  will be two wood in the list *)
let getMineResLists (game:game) (me:gPlayer) : resource list = 
  snd(leftFoldHexList (fun (index, acc) hex -> 
    if (memPlayerList me (getSurroundedPlayer game index))
      then 
        let (ter, roll) = hex in
        let res = resource_of_terrain ter in
        (match res with 
        | Some r -> (index+1, r::acc)
        | None -> (index+1, acc))
    else (index+1, acc)
  ) (0, []) game.gHexList)

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
let assignPointOnTerType (ter : terrain) (mineRes: resource list) : int = 
  match resource_of_terrain ter with
  | None -> 0
  | Some res -> 
    begin
      match res with
      | Brick | Lumber -> if(List.mem res mineRes) then 2 else 3
      | Wool | Ore | Grain -> if(List.mem res mineRes) then 0 else 1
    end

(* Give a list of terrain points. 
  Criterion:  If resources are different from each other, 
  give additional 1 points, besides base points. *)
let assignPointOnTer (ters : terrain list) (mineRes: resource list) : int = 
  let basePoints = List.fold_left 
  (fun sum ter -> sum + (assignPointOnTerType ter mineRes)) 0 ters in
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
  let mineRes = getMineResLists game (findPlayer game game.gActive) in
  (* points assigned based on the roll number of adjacent hexes *)
  let rollPoint = assignPointOnRoll rollList in
  let terPoint = assignPointOnTer terList mineRes in
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
  print_string "all suitableTown are ";
  print_endline (string_of_list (string_of_int) allSuitable);
  let res = fst(assignIntersPoints game allSuitable) in
  print_int res; print_endline " as best"; res

(* return the best place for building a road *)
let findBestRoadLocation (game:game) : line = 
  failwith "unimplemented findBestRoadLocation"


(**********************************************************************)
(******              {Player related helper functions}           ******)
(**********************************************************************)

(*NOT ENTIRELY ACCURATE, does not accoutn for victory points in players' hands*)
let checkVictoryPointsBot (g:game) (p:gPlayer) : int = 
  let color = getPlayerColor p in 

  let vpTowns = sizeOfInterList (filterOnInterList
                            (fun ele -> ele = Some(color,Town)) g.gInterList) in 
  let vpCities = sizeOfInterList (filterOnInterList
                            (fun ele -> ele = Some(color,City)) g.gInterList) in 
  let vpLargestArmy = if p.gPLargestarmy then 1 else 0 in 
  let vpLongestroad = if p.gPLongestroad then 1 else 0 in 
  (cVP_TOWN * vpTowns) +
  (cVP_CITY * vpCities) + 
  (cVP_LARGEST_ARMY * vpLargestArmy) + 
  (cVP_LONGEST_ROAD * vpLongestroad)



(* assign point on a certain hex.
  Criterion: 1. roll number 2. resource type: number of surrounding players*)
let assignPointOnHex (game:game) (hIndex:int) : int = 
  let (ter, roll) = nthOfHexList game.gHexList hIndex in
  let rollPoint = assignPointOnRollNumber roll in
  let mineRes = getMineResLists game (findPlayer game game.gActive) in
  let terPoint = assignPointOnTerType ter mineRes in
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
      let curVicPoints = checkVictoryPointsBot game player in
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





(* return the color of a player with the max amount of certain resource.
  If all player don't have that resource, return None *)
let findPlayerWithMaxRes (game:game) (res:resource) : color option = 
  let me = game.gActive in
  let pList = game.gPlayerList in
  let res = leftFoldPlayerList (fun (color, max) player -> 
    let num = num_resource_in_inventory player.gPInventory res in
    let curColor = getPlayerColor player in
    if(curColor = me) then (color, max)
    else if(num > max) then (curColor, num) else (color, max)
  ) (next_turn me, 0) pList in
  if(snd res = 0) then None else Some (fst res)


(* return whether the player has enough resource for trading.
  Condition, sum_cost > 4 *)
let hasEnoughResTrade (game:game) : bool = 
  let mePlayer = findPlayer game game.gActive in
  let inv = mePlayer.gPInventory in
  sum_cost inv >= 4

(* find mine current max resource type *)
let findMineMaxResNotWanted (game:game) (wanted:resource) : resource = 
  let me = game.gActive in
  let mePlayer = findPlayer game me in
  let myInv = mePlayer.gPInventory in
  let lst = costToPairList myInv in
  fst(List.fold_left (fun (maxRes, max) (res, num) -> 
    if(res = wanted) then (maxRes, max)
    else 
      if(num > max) then (res, num) 
      else (maxRes, max)) (Grain, 0) lst )


(* generate domestic trade request *)
(* Strategy: find the resource need most -> see if the maritime 
  trade ratio is acceptable, if not trade with other player. If 
  after all domestic trade been made, still have more than max 
  number of resource can have, trade with maritime trade *)


(* find the resource we want to trade for *)
let findWantedResource (game:game) : resource = 
  let me = game.gActive in
  let mePlayer = findPlayer game me in
  let deltaTown = deltaResourceNeed mePlayer (BuildTown 0) in
  let deltaRoad = deltaResourceNeed mePlayer (BuildRoad (me, (0,0))) in
  let deltaCity = deltaResourceNeed mePlayer (BuildCity 0) in
  let deltaCard = deltaResourceNeed mePlayer BuildCard in
  let settleNumber = settlementNumber game me in
  (* return the smaller one of among two costs, smaller means with
    smaller sum in this case. *)
  let minSum (cost1:cost) (cost2:cost) : cost = 
    if(sum_cost cost1 < sum_cost cost2) then cost1
    else cost2
  in
  let minResInCost (cost:cost) : resource = 
    let rec find (cost:cost) : resource = 
      (match cost with
      | (0, _, _, _, _) -> Brick
      | (_, 0, _, _, _) -> Wool
      | (_, _, 0, _, _) -> Ore
      | (_, _, _, 0, _) -> Grain
      | (_, _, _, _, 0) -> Lumber
      | _ -> find (minusCosts cost (1, 1, 1, 1, 1)) )
    in
    find cost
  in
  let minCost = 
    if(settleNumber < 4) then minSum deltaTown deltaCity
    else minSum (minSum deltaTown deltaCity) (minSum deltaRoad deltaCard)
  in
  minResInCost minCost


(* genereate actions after cards play phase in a bot *)
let generateActionAfterCard (game:game) : action = 
  let me = game.gActive in
  let mePlayer = findPlayer game me in
  if(is_none game.gDiceRolled) then RollDice
  else 
    if((hasEnoughResBuild (BuildTown 0) mePlayer )
        && (settlementNumber game me < 4))
      then 
        let town = findBestTownLocation game in
        BuyBuild (BuildTown(town))
    else
      let wantedRes = findWantedResource game in
      let playerWithMax = findPlayerWithMaxRes game wantedRes in
      (* trade constraints: both me and the player we want to trade
      has enough resource for sell and buy and the trade turns is less
      than the max number *)
      if (playerWithMax != None 
        && hasEnoughResTrade game 
        && game.gTradesMade < cNUM_TRADES_PER_TURN)
          then 
            let color = get_some playerWithMax in
            let sell = findMineMaxResNotWanted game wantedRes in
            DomesticTrade 
            (color, (single_resource_cost wantedRes), 
                    (single_resource_cost sell)      ) 
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
  => try some trade, if resource exceed max, after trade is no longer 
      available, try maritime trade.
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





















