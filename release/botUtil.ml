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
(******              {Discard phase helper functions}            ******)
(**********************************************************************)
let genDiscardMove (g:game) : move = 
  let discardingPlayer =  findPlayer g g.gNextColor in
  if sum_cost discardingPlayer.gPInventory <= cMAX_HAND_SIZE
  then (
    (* print_endline "no discard"; *)
       DiscardMove((0,0,0,0,0)) )
  else (let cost = discardHelper discardingPlayer.gPInventory (0,0,0,0,0) 
                    ((sum_cost discardingPlayer.gPInventory) / 2) in
  (* print_string ("discarding: " ^ string_of_cost cost ^ "\n"); *)
  DiscardMove(cost))




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
  (* print_string "all suitableTown are ";
  print_endline (string_of_list (string_of_int) allSuitable); *)
  let res = fst(assignIntersPoints game allSuitable) in
  (* print_int res; print_endline " as best";  *)
  res




(* return the max length from current point and the end point that
  has the max length *)
let maxLengthAndEnd (game:game) (me:color) (point:point)  : (point*int) = 
  (* make line always in asending order *)
  
  let ascendingLine (p1, p2) = 
    if(p1 < p2) then (p1, p2) else (p2, p1)
  in
  (* make road always in ascending order *)
  let ascendRoadList = mapRoadList 
    (fun (c, line) -> (c, ascendingLine line)) game.gRoadList in

  (* print_endline "ascendRoadList";
    print_endline (string_of_list 
      (fun (c, (p1, p2)) -> 
        (string_of_color c) ^ (string_of_int p1) ^ (string_of_int p2))
    ascendRoadList); *)



  let rec maxAndEnd (roadList:road list) (cur:point) (me:color)
          (prev:line list) (length:int) : (point*int) = 
    (* print_string ("max function color is " ^ (string_of_color me));
    print_endline "";
    print_string "max function cur is "; print_int cur;
    print_endline "";
    print_endline "prev";
    print_endline (string_of_list 
      (fun (p1, p2) -> (string_of_int p1) ^ (string_of_int p2))
    prev); *)
    let possibleRoadsFromCur = 
      List.map (fun p -> ascendingLine(cur, p)) (adjacent_points cur)
    in
    (* print_endline "possibleRoadsFromCur";
    print_endline (string_of_list 
      (fun (p1, p2) -> (string_of_int p1) ^ (string_of_int p2))
    possibleRoadsFromCur); *)
    (* return all mine road from current point *)
    let mineRoadFromCur = List.filter 
      (fun line -> List.mem (me, line) roadList) possibleRoadsFromCur 
    in
    (* print_endline "mineRoadFromCur";
    print_endline (string_of_list 
      (fun (p1, p2) -> (string_of_int p1) ^ (string_of_int p2))
    mineRoadFromCur); *)
    let mineRoadWithoutDup = List.filter
      (fun line -> not (List.mem line prev)) mineRoadFromCur 
    in
    (* print_endline "mineRoadWithoutDup";
    print_endline (string_of_list 
      (fun (p1, p2) -> (string_of_int p1) ^ (string_of_int p2))
    mineRoadWithoutDup); *)
    if(List.length mineRoadWithoutDup = 0) then (cur, length)
    else 
      let tempRes = List.map (fun line -> 
        let (p1, p2) = line in
        if(p1 = cur) 
          then maxAndEnd roadList p2 me (line::prev) (length+1)
        else maxAndEnd roadList p1 me (line::prev) (length+1))
      mineRoadWithoutDup
      in
      List.fold_left (fun (maxP, maxLen) (p, length) -> 
        (* print_string "maxP "; print_int maxP;
        print_endline "";
        print_string "maxLen"; print_int maxLen;
        print_endline "";
        print_string "p "; print_int p;
        print_endline "";
        print_string "length"; print_int length;
        print_endline ""; *)
        if(length > maxLen) then (p, length) else (maxP, maxLen)
      ) (0, -1) tempRes
  in
  maxAndEnd ascendRoadList point me [] 0

(* return the max length of a  *)
let findMaxLenEnd (game:game) (me:color) : (point*int) = 
  let mineInter = snd (leftFoldInterList (fun (index, acc) inter -> 
    match inter with
    | None -> (index+1, acc)
    | Some (c, _) -> 
      if(c = me) then (index+1, index::acc) else (index+1, acc)
  ) (0, []) game.gInterList) in
  let maxEndList = List.map (maxLengthAndEnd game me) mineInter in
  let tempRes = List.fold_left 
    (fun (maxP, maxLen) (p, length) -> 
        if(length > maxLen && (buildableRoad game p != None)) 
          then (p, length) else (maxP, maxLen)
    ) (0, -1) maxEndList
  in
  (* print_endline ("cur color is " ^ (string_of_color me));
  print_string "length is "; print_int (snd tempRes);
  print_string "  end is "; print_int (fst tempRes);
  print_endline ""; *)
  tempRes




(* return the best place for building a road *)
let findBestRoadLocation (game:game) : line = 
  let tempRes = findMaxLenEnd game game.gActive in
  (* print_string "road end point is "; print_int (fst tempRes);
  print_endline "";
  print_string "current max length is  "; print_int (snd tempRes); 
  print_endline ""; *)
  get_some (buildableRoad game (fst tempRes))
 

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
    let pList = getSurroundedPlayer game index in
    if(List.length pList = 0 
      || List.mem (findPlayer game me) pList) then (index+1, acc)
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
  (* print_endline ("wanted resource is " ^ (string_of_resource res)); *)
  let me = game.gActive in
  let pList = game.gPlayerList in
  let res = leftFoldPlayerList (fun (color, max) player -> 
    let num = num_resource_in_inventory player.gPInventory res in
    let curColor = getPlayerColor player in
    if(curColor = me) then (color, max)
    else if(num > max) then (curColor, num) else (color, max)
  ) (next_turn me, 0) pList in
  if(snd res = 0) then None 
  else 
    (
      (* print_endline ("player with max is " ^ string_of_color (fst res)); *)
    Some (fst res))


(* return whether the player has enough resource for trading.
  Condition, sum_cost > 4 *)
let hasEnoughResTrade (game:game) : bool = 
  let mePlayer = findPlayer game game.gActive in
  let myInv = mePlayer.gPInventory in
  let lst = costToPairList myInv in
  let mineResMax = 
    snd (List.fold_left (fun (maxRes, max) (res, num) -> 
      if(num > max) then (res, num) 
      else (maxRes, max)) (Grain, 0) lst ) 
  in
  sum_cost myInv >= 4 && mineResMax > 1


(* find mine current max resource type *)
let findMineMaxResNotWanted (game:game) (wanted:resource) : (resource*int) = 
  let me = game.gActive in
  let mePlayer = findPlayer game me in
  let myInv = mePlayer.gPInventory in
  let lst = costToPairList myInv in
  (List.fold_left (fun (maxRes, max) (res, num) -> 
    if(res = wanted) then (maxRes, max)
    else 
      if(num > max) then (res, num) 
      else (maxRes, max)) (Grain, 0) lst )

(* return wether has enough resource for maritime trade.
  Max resource in hand beside wanted resource exceed trade ratio+1*)
let hasEnoughResMariTrade (game:game) (wanted:resource) :bool = 
  let me = game.gActive in
  let ratio = getMariTradeRatio game wanted me in
  let numHas = snd (findMineMaxResNotWanted game wanted) in
  numHas > ratio + 1




(* generate domestic trade request *)
(* Strategy: find the resource need most -> see if the maritime 
  trade ratio is acceptable, if not trade with other player. If 
  after all domestic trade been made, still have more than max 
  number of resource can have, trade with maritime trade *)


(* find the resource we want to trade for *)
let findWantedResource (game:game) : resource option = 
  let me = game.gActive in
  let mePlayer = findPlayer game me in
  let deltaTown = deltaResourceNeed mePlayer (BuildTown 0) in
  let deltaRoad = deltaResourceNeed mePlayer (BuildRoad (me, (0,0))) in
  let deltaCity = deltaResourceNeed mePlayer (BuildCity 0) in
  (* let deltaCard = deltaResourceNeed mePlayer BuildCard in *)
  let settleNumber = settlementNumber game me in
  (* return the smaller one of among two costs, smaller means with
    smaller sum in this case. *)
  let minSum (cost1:cost) (cost2:cost) : cost = 
    if(sum_cost cost1 < sum_cost cost2) then cost1
    else cost2
  in
  let maxResInCost (cost:cost) : resource option = 
    let lst = costToPairList cost in
    let tempRes = (List.fold_left (fun (maxRes, max) (res, num) -> 
      if(num > max) then (res, num) 
      else (maxRes, max)) (Grain, 0) lst ) in
    if(snd tempRes = 0) then None else Some (fst tempRes)
  in
  let minCost = 
    if(settleNumber < 4) then deltaTown
    (* else minSum (minSum deltaTown deltaCity) (minSum deltaRoad deltaCard) *)
  else minSum (minSum deltaTown deltaCity) deltaRoad 
  in
  (* print_endline ("wanted delta cost" ^ (string_of_cost minCost)); *)
  maxResInCost minCost



(* return Some availabe town or None *)
let availableTown (game:game) (me:color) : point option = 
  let mineTowns = snd (leftFoldInterList (fun (index, acc) inter -> 
    (match inter with
    | None -> (index+1, acc)
    | Some (c, settlement) -> 
      if(c = me && settlement = Town) then (index+1, index::acc)
      else (index+1, acc))
  ) (0, []) game.gInterList)
  in
  if List.length mineTowns = 0 then None
  else 
    let tempRes = assignIntersPoints game mineTowns in
    Some (fst tempRes)



(* genereate actions after cards play phase in a bot *)
let generateActionAfterCard (game:game) : action = 
  let me = game.gActive in
  let mePlayer = findPlayer game me in
  if(is_none game.gDiceRolled) then RollDice
  else 
    if((hasEnoughResBuild (BuildTown 0) mePlayer )
        && (settlementNumber game me < 4
          || (checkVictoryPointsBot game mePlayer) >=6  ))
      then
        let town = findBestTownLocation game in
        BuyBuild (BuildTown(town))
    else
      let wantedRes = findWantedResource game in
      let playerWithMax = 
        if (wantedRes != None) 
          then findPlayerWithMaxRes game (get_some wantedRes)
        else None in
      (* trade constraints: both me and the player we want to trade
      has enough resource for sell and buy and the trade turns is less
      than the max number *)
      if (wantedRes != None
        && playerWithMax != None 
        && hasEnoughResTrade game 
        && game.gTradesMade < cNUM_TRADES_PER_TURN)
          then 
            let color = get_some playerWithMax in
            let sell = 
              fst (findMineMaxResNotWanted game (get_some wantedRes)) in
            DomesticTrade 
            (color, (single_resource_cost sell), 
              (single_resource_cost (get_some wantedRes)) ) 
      else 
        let line = findBestRoadLocation game in
        if ((hasEnoughResBuild (BuildRoad (me, (0,0))) mePlayer)
          && ((settlementNumber game me) >= 4) 
          && suitableRoad game (me, line))
          then 
            BuyBuild (BuildRoad(me, line))
        else 
          let city = availableTown game me in
          if ((hasEnoughResBuild (BuildCity 0) mePlayer)
              && (city != None))
            then BuyBuild (BuildCity(get_some city))
          else 
          EndTurn


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





















