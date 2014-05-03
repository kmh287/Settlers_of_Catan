(* open Definition
open Constant
open Util
open Print
include Model  *)

(**********************************************************************)
(******              {Build related helper functions}            ******)
(**********************************************************************)

(* return list all possible adjacent roads with color equals to the
current active player *)
let all_adjacent_curColor_road (g:game) (pt:point) : road list = 
  mapLineList (fun adPt -> (g.gActive, (pt, adPt))) (adjacent_points pt)

(* helper function used to check whether a road already exists *)
let existsRoad (g:game) (road:road) : bool = 
  let (c, (p1, p2)) = road in
  let revRoad = (c, (p2, p1)) in
  not( memRoadList road g.gRoadList) 
  && not (memRoadList revRoad g.gRoadList)

(*Helper function to check if road is suitable. Need to consider the 
  whether the road has already been built(both (p1,p2)&&(p2, p1) order)
  and whether this new road is connect to a exsiting road belongs to the
  player, return false if road is not valid *)
let suitableRoad (g:game) (road:road) : bool = 
  let (c, (p1, p2)) = road in
  (* there are duplicates and road want to add in this 
    combined list, but doesn't matter *)
  let adjacentRoads = appendRoadLists 
    (all_adjacent_curColor_road g p1) (all_adjacent_curColor_road g p2) in
  not (existsRoad g road) && existsRoadList (existsRoad g) adjacentRoads

(* Helper function to check if town is valid, need to consider if there 
is not town already exists on that point and there are no town that is 
adjacent to it in one road length, return false if not valid *) 
let suitableTown (g:game) (town:point) : bool = 
  let interList = g.gInterList in
  (nthOfInterList interList town = None) && 
  (forAllLineList (fun p -> (nthOfInterList interList p) = None) 
    (adjacent_points town) )

(* Helper function used to check whether building a city on a point is
valid. Need to check that there is already a town on that point and
that the town belongs to the current active player, return false if not 
valid build site for city *)
let suitableCity (g:game) (city:point) : bool = 
  let interList = g.gInterList in
  match (nthOfInterList interList city) with
  | Some (c, settlement) -> c = g.gActive && settlement = Town
  | _ -> false

(*Helper function to check if pt && points adjacent to pt are unsettled*)
let suitableSettlementPoint (g:game) (pt:point) : bool = 
  (*Replaced by suitableTown rhelper, 
  kept in for the moment to keep code working*)
  suitableTown g pt 

(*Function to search for a point that does not have 
have a settlement nor adjacent settlements*)
let settleablePoint (g:game) : point= 
    (*Find index of first element that is settleable in interlist*)
    let intersectionIndices = (mapiInterList (fun i element -> i) g.gInterList) in 
    list_indexof (fun boolean -> boolean)
        (List.map (fun ele -> suitableSettlementPoint g ele) intersectionIndices) 

(*Function to return an unoccupied line with one end at pt *)
let buildableRoad (g:game) (pt:point) : line = 
    let occupiedLines = mapRoadList(fun (color,line) -> line) g.gRoadList in 
    let possibleRoads = mapLineList (fun ele -> (pt,ele) ) (adjacent_points pt) in
    findRoadList (fun ele -> (not) (memRoadList ele occupiedLines)) possibleRoads

(**********************************************************************)
(******              {initial_move helper functions}             ******)
(**********************************************************************)

(*Count the total number of settlements (TOWNS AND CITIES) on the board*)
let countSettlements g = 
    list_count (fun ele -> if ele = None then false else true) g.gInterList

(*Return a list of indices of locations of this color's settlements*)
let findPlayerSettlements (g:game) (col:color) : point list =
  let interList = g.gInterList in
  let uncleanIndexList = mapiInterList
                  (fun index ele -> match ele with
                   |Some (color,settlement) -> if color = col then index else -1
                   |None -> -1 ) interList in 
  List.fold_left (fun acc ele -> if ele <> -1
                                   then ele::acc
                                   else acc) [] uncleanIndexList

(*Resource updater FOR INIT PHASE*)
let initUpdateResources g color : gPlayer = 
    (*Map over the list, turn all points that don't belong to color to -1*)
    let indexList = findPlayerSettlements g color in 
    (*Find all pieces adjacent to the indices in indexList*)
    let pieceList = List.flatten (List.map (fun ele ->
                                         adjacent_pieces ele) indexList) in
    (*Lookup piece numbers in hex list, and build list of terrain types*)
    let terrainList = List.map (fun ele -> 
                                    fst(List.nth g.gHexList ele)) pieceList in 
    let rec resourceGatherer (tlist:terrain list) (acc:cost) : cost = 
        match tlist with 
            |[] -> acc
            |hd::tl -> let rsource = resource_of_terrain hd  in 
                       if rsource = None 
                       (*No resource, then move down the list *)
                       then resourceGatherer tl acc
                       (*If there is a resource, add the cost to the acc*)
                       else let newResources =
                         map_cost (fun ele -> ele * cRESOURCES_GENERATED_TOWN)
                                  (single_resource_cost (get_some rsource))in  
                       (*Call again on tail, with acc + new resources*)
                       resourceGatherer tl 
                                    (addCosts acc newResources)  in 
    let totalNewResources = resourceGatherer terrainList (0,0,0,0,0) in 

    (*Deconstruct player list, then build it again with new resources added*)
    let player = findPlayer g color in 
    {player with gPInventory = addCosts player.gPInventory totalNewResources;}


(**********************************************************************)
(******               {robber_move helper functions}             ******)
(**********************************************************************)



(**********************************************************************)
(******                {discard helper functions}                ******)
(**********************************************************************)



(**********************************************************************)
(******              {trade response helper functions}           ******)
(**********************************************************************)



(**********************************************************************)
(******              {RollDice helper functions}                 ******)
(**********************************************************************)

(* return (hex,inter) pair list that satisfy the rolled number 
  and there is no robber on that hex *)
let rolledHexInterPair (g:game) (roll:roll) : (hex * intersection) list = 
  let interList = g.gInterList in
  snd(
    leftFoldHexList (fun (index, acc) (t, r) -> 
      if (r = roll && index != g.gRobber) then 
        begin
          let adjacentPointList = piece_corners index in
          let newPair = (leftFoldPointList
            (fun pairs point -> 
              ((t, r), (nthOfInterList interList point))::pairs) 
            [] adjacentPointList)
          in
          (index+1, List.append newPair acc)  
        end
      else (index+1, acc)) (0, []) g.gHexList)


(* update the player's resource based on hex, inter pair, return
  a updated game status *)
let addResToPlayer (game:game) ((hex, inter):(hex * intersection)):game = 
  let (ter, r) = hex in
  match (inter, resource_of_terrain ter) with
  | Some(color, settlement), Some res ->  
    begin
      let curBaseRes = single_resource_cost res in 
      let curAddRes = multiRes 
        (settlement_num_resources settlement) curBaseRes in
      let curPlayer = findPlayer game game.gActive in
      let newInv = addCosts curAddRes curPlayer.gPInventory in
      let newPlayer = {curPlayer with gPInventory = newInv} in
      updatePlayer game newPlayer
    end
  | _ -> game
    

(* generate sources to all players and update game status *)
let generateResource (g : game) : game = 
  match g.gDiceRolled with
  | None -> g
  | Some roll -> 
      (* this is a (hex * inter) list *)
      let hexInterPairs = rolledHexInterPair g roll in
      List.fold_left addResToPlayer g hexInterPairs


(**********************************************************************)
(******              {MariTrade helper functions}                ******)
(**********************************************************************)

(* return the ratio of specific player and resource type *)
let getMariTradeRatio 
    (game:game) (res:resource) (player:color) : ratio = 
  let portList = game.gPortList in
  let interList = game.gInterList in
  (* if current port match on player and resource type, return
  its ratio, otherwise return default mariTime trade ratio *)
  let portRatio (port:port) : ratio =
    let ((p1, p2), ratio, pRes) = port in 
    if((pRes = PortResource(res) || pRes = Any) 
      && (
        (interMatchPlayer (nthOfInterList interList p1) player) 
        || (interMatchPlayer (nthOfInterList interList p2) player)
      )) then ratio
    else cMARITIME_DEFAULT_RATIO
  in
  leftFoldPortList (fun acc e -> min acc (portRatio e))
    cMARITIME_DEFAULT_RATIO portList
    

(* update inventory based on sell type but type and trade ratio,
if this exchange is not valid(amount of sell type is less than ratio),
return original inventory *)
let updateInventory (sell:resource) (buy:resource) (ratio:ratio) 
      (inv:inventory) : inventory = 
  let hasSellAmount = 
    num_resource_in_inventory inv sell in       
    (* if resource in hand is less than ratio, do nothing *)
  if(hasSellAmount < ratio) then inv
  else 
    let minusSell = 
      increaseResInInventory inv sell (-ratio) in
    increaseResInInventory minusSell buy 1
    

(**********************************************************************)
(******              {PlayCard helper functions}                 ******)
(**********************************************************************)

(* return the card type of specific playcard type *)
let cardOfPlaycard (pc:playcard) : card = 
  match pc with
  | PlayKnight _ -> Knight
  | PlayRoadBuilding _ -> RoadBuilding
  | PlayYearOfPlenty _ -> YearOfPlenty
  | PlayMonopoly _ -> Monopoly


(**********************************************************************)
(******              {BuyBuild helper functions}                 ******)
(**********************************************************************)

(* function used to build road and return a updatde game status *)
let buildRoad (game:game) (road:road) : game = 
  let cost = cCOST_ROAD in
  let curPlayer = findPlayer game game.gActive in
  let origInv = curPlayer.gPInventory in
  (* check if there is already road build on that point, or the 
  player has enough resource to build the road. If not, 
  return current game status *)
  if (not(suitableRoad game road) 
      || not (greaterThanEqual origInv cost) )
    then game
  else
    let updatedPlayer = {curPlayer with 
      gPInventory = minusCosts origInv cost;
    } in
    let updatedPGame = updatePlayer game updatedPlayer in
    {updatedPGame with
      gRoadList = addToRoadList road game.gRoadList;}

(* function used to build town and return a updatde game status *)
let buildTown (game:game) (point:point) : game = 
  let cost = cCOST_TOWN in
  let curPlayer = findPlayer game game.gActive in
  let origInv = curPlayer.gPInventory in
  (* check if there that point is suitable for builing a town, 
  or the player has enough resource to build the road. If not, 
  return current game status *)
  if (not(suitableTown game point) 
      || not (greaterThanEqual origInv cost) )
    then game
  else
    let updatedPlayer = {curPlayer with 
      gPInventory = minusCosts origInv cost;
    } in
    let updatedPGame = updatePlayer game updatedPlayer in
    let newTown = Some (game.gActive, Town) in
    {updatedPGame with
      gInterList = setNthInterList point newTown game.gInterList;}

(* function used to build city and return a updatde game status *)
let buildCity (game:game) (point:point) : game =
  let cost = cCOST_CITY in
  let curPlayer = findPlayer game game.gActive in
  let origInv = curPlayer.gPInventory in
  (* check if there that point is suitable for builing a city, 
  or the player has enough resource to build the road. If not, 
  return current game status *)
  if (not(suitableCity game point) 
      || not (greaterThanEqual origInv cost) )
    then game
  else
    let updatedPlayer = {curPlayer with 
      gPInventory = minusCosts origInv cost;
    } in
    let updatedPGame = updatePlayer game updatedPlayer in
    let newCity = Some (game.gActive, City) in
    {updatedPGame with
      gInterList = setNthInterList point newCity game.gInterList;}

(* function used to build card and return a updatde game status. 
  Noticed that the newly bought card is only added to cardBought
  but not player's hand, it's only added to player's hand at the
  end of this turn *)
let buildCard (game:game) : game = 
  (* if deck is empty, then return original game status *)
  match game.gDeck with
  | Hidden _ -> game
  | Reveal cList -> 
      if(checkCardListNull cList) then game
      else
        let (draw, remain) = pick_one cList in
        (* let curPlayer = findPlayer game game.gActive in
        let updatedPlayer = 
          {curPlayer with gPCard = (addToCard draw curPlayer.gPCard)} in
        let updatedPGame = updatePlayer game updatedPlayer in *)
        {game with
          gDeck = Reveal remain;
          gCardsBought = addToCard draw game.gCardsBought;
        }  

(**********************************************************************)
(******               {EndTurn helper functions}                 ******)
(**********************************************************************)

(* find the next player should take turn *)
let findNextPlayer (game:game) : color = 
  let curPlayerColor = game.gActive in
  next_turn curPlayerColor

(* reset turn section of game status for next turn *)
let resetGameTurn (game:game) : game = 
  {game with
    gDiceRolled = None;
    gCardPlayed = false;
    gCardsBought = Reveal [];
    gTradesMade = 0;
    gPendingTrade = None;
  }

(* generate the next game status for next turn, reset next section and 
 add cards that are bought in this turn into player's hand *)
let nextTurnGame (game:game) : game = 
  let curPlayer = findPlayer game game.gActive in
  let updatedPlayer = 
    {curPlayer with 
      gPCard = (addCardsToCards game.gCardsBought curPlayer.gPCard);} in
  let updatedPGame = updatePlayer game updatedPlayer in
  let resetGame = resetGameTurn updatedPGame in 
  let nextPlayerColor = findNextPlayer game in
  {resetGame with
    gActive = nextPlayerColor;
    gNextColor = nextPlayerColor;
    gNextRequest = ActionRequest;
  }

(* function used to check whether there is a winner *)
let checkWinner (game:game) : color option = 
  failwith "checkWin unimplemented"
  
(**********************************************************************)
(******                         SCRUBBER                         ******)
(**********************************************************************)

let surroundingColors (g:game) (piece:piece) : color option list = 
  let surroudningInters = piece_corners piece in 
  List.map (fun ele -> 
    let settlementOption = nthOfInterList g.gInterList ele in 
    if settlementOption = None 
    then None 
    else Some (fst(get_some settlementOption)) ) surroudningInters

let surroundingColorsNoOptions (g:game) (piece:piece) : color list = 
  let surroundingColorsOptions = surroundingColors g piece in 
  List.fold_left (fun acc ele -> if ele = None 
                                 then acc 
                                 else (get_some ele)::acc) 
                    [] surroundingColorsOptions

(*Valid IFF pt1 is unsettled and (pt1,pt2) is an unbuilt and suitable move*)
let validInitialMove (g:game) (pt1:point) (pt2:point) : bool = 
  suitableTown g pt1 && not (existsRoad g (g.gActive,(pt1,pt2)))

(*Valid IFF colorOption is adjacent to piece and is not the active player*)
let validRobberMove (g:game) (piece:piece) (colorOption:color option) : bool = 
  List.mem colorOption (surroundingColors g piece) 
  &&
  (*Ensure player does not select himself/herself*)
  (colorOption = None || get_some colorOption <> g.gActive) 

(*Valid IFF the *)
let validDiscardMove (g:game) (cost:cost) : bool = 
  let discardingPlayer = findPlayer g g.gNextColor in 
  validCost (minusCosts discardingPlayer.gPInventory cost)

let genMinInitialMove (g:game) : move = 
  let settlementPoint = settleablePoint g in 
  let roadLine = buildableRoad g settleablePoint in 
  InitialMove(settlementPoint,roadLine)

let genMinRobberMove (g:game) : move = 
  let piece = Random.int (cMAX_PIECE_NUM) in 
  let colorOption = pick_random (surroundingColorsNoOptions g piece) in 
  RobberMove(piece,colorOption) 

(* 
let genMinDiscardMove (g:game) : move = 
  let discardingPlayerColor = g.gNextColor in 
  let discardingPlayer =  foo in  *)




(* CONS: whether the dice has been rolled *)
let validRollDice (game:game) : bool = game.gDiceRolled = None

(* CONS: player active the trade has enough that kind of resource*)
let validMariTrade (game:game) (mtrade:maritimetrade) : bool = 
  let curPlayer = findPlayer game game.gActive in
  let (sell, buy) = mtrade in
  let ratio = getMariTradeRatio game sell game.gActive in
  let origInv = curPlayer.gPInventory in
  let hasSellAmount = num_resource_in_inventory origInv sell in
  hasSellAmount >= ratio

(* CONS trade time doesn't exceed or equal to the max number *)
let validDomesticTrade (game:game) : bool = 
  game.gTradesMade < cNUM_TRADES_PER_TURN

(* CONS road want to build is suitable and the player has 
  enough resource for building that road *)
let validBuildRoad (game:game) (road:road) : bool = 
  let cost = cCOST_ROAD in
  let curPlayer = findPlayer game game.gActive in
  let origInv = curPlayer.gPInventory in
  (suitableRoad game road) && (greaterThanEqual origInv cost)

(* CONS town want to build is suitable and the player has 
  enough resource for building that town. *)
let validBuildTown (game:game) (town:point) : bool = 
  let cost = cCOST_TOWN in
  let curPlayer = findPlayer game game.gActive in
  let origInv = curPlayer.gPInventory in
  (suitableTown game town) && (greaterThanEqual origInv cost)

(* CONS city want to build is suitable and the player has 
  enough resource for building that city *)
let validBuildCity (game:game) (city:point) : bool = 
  let cost = cCOST_CITY in
  let curPlayer = findPlayer game game.gActive in
  let origInv = curPlayer.gPInventory in
  (suitableCity game city) && (greaterThanEqual origInv cost)

(* CONS the deck is not empty and the cards player already has
  is less than the max number *)
let validBuildCard (game:game) : bool = 
  let curPlayer = findPlayer game game.gActive in
  match game.gDeck with
  | Hidden _ -> false
  | Reveal cList -> 
    not (checkCardListNull cList) 
      && (sizeOfCards curPlayer.gPCard) < cMAX_HAND_SIZE

(* CONS: the player should have that card in hand, and the 
  card action he specific is also valid *)
let validPlayCard (game:game) (pCard:playcard) : bool = 
  let card = cardOfPlaycard pCard in
  let curPlayer = findPlayer game game.gActive in
  let hasCard = memCards card (curPlayer.gPCard) in
  match pCard with
  | PlayKnight (piece, color) -> 
      hasCard && (validRobberMove game piece color)
  | PlayRoadBuilding (road1, roadOp) ->
      hasCard && (suitableRoad game road1) 
        && (roadOp = None || suitableRoad game (get_some roadOp))
  | _ -> hasCard



(* CONS: dice has been rolled *)
let validEndTurn (game:game) : bool = game.gDiceRolled != None


let genMinMove (g:game) (request:request) : move = 
  failwith "unimplemented"

let scrubMove (game:game) (move:move) : move = 
  let request = game.gNextRequest in 
  match (move,request) with
    |InitialMove(pt1,pt2),InitialReQuest ->
      if validInitialMove(game,pt1,pt2)  
      then move 
      else genMinInitialMove game  

    |RobberMove(piece,colorOption),RobberRequest -> 
      if validRobberMove(game,piece,colorOption)
      then move 
      else genMinRobberMove game 

    |DiscardMove(cost),DiscardRequest -> 
      if validDiscardMove(game,cost) 
      then move 
      else genMinDiscardMove game
    |TradeResponse(resp),TradeRequest -> failwith "unimplemented"
    |Action(action),ActionRequest ->
      begin
        match action with
        | RollDice ->  
          if validRollDice game then move
          else genMinMove game ActionRequest
        | MaritimeTrade mtrade -> 
          if (validMariTrade game mtrade) then move
          else genMinMove game ActionRequest
        | DomesticTrade trade -> 
          if (validDomesticTrade game) then move
          else genMinMove game ActionRequest
        | BuyBuild build -> 
          begin
            match build with
            | BuildRoad road -> 
              if (validBuildRoad game road) then move
              else genMinMove game ActionRequest
            | BuildTown town -> 
              if (validBuildTown game town) then move
              else genMinMove game ActionRequest
            | BuildCity city -> 
              if (validBuildCity game city) then move
              else genMinMove game ActionRequest
            | BuildCard -> 
              if (validBuildCard game) then move
              else genMinMove game ActionRequest
          end
        | PlayCard playcard ->
          if(validPlayCard game playcard) then move
          else genMinMove game ActionRequest
        | EndTurn -> 
          if(validEndTurn game) then move
          else genMinMove game ActionRequest
      end
    |_ -> genMinMove game request 




<<<<<<< HEAD











=======
  match (move,reqeust) with
    |InitialMove(pt1,pt2),InitialReQuest ->
      if validInitialMove(game,pt1,pt2)  
      then move 
      else genMinInitialMove game  

    |RobberMove(piece,colorOption),RobberRequest -> 
      if validRobberMove(game,piece,colorOption)
      then move 
      else genMinRobberMove game 

    |DiscardMove(cost),DiscardRequest -> 
      if validDiscardMove(game,cost) 
      then move 
      else genMinDiscardMove game 

                          (*CONSTAINTS:    - Player must be able to afford cost

                           SOLUTIONS      - If player cannot afford cost, then 
                                            try anothe resource. If player has 
                                            no resources, d not take anything.*)

    |TradeResponse(resp),TradeRequest  = (*CONSTRAINTS:       - Both players must be able to afford
                                             the trade 

                        SOLUTIONS:          - If either player cannot afford, 
                                              set response to FALSE.*)

    |Action(action),ActionRequest=            (*CONSTRAINTS: XIAO MING FILL THIS IN
                        SOLUTIONS:   XIAO MING FILL THIS IN *) 
    |_ -> genMinMove g request


(*Valid IFF pt1 is unsettled and (pt1,pt2) is an unbuilt and suitable move*)
let validInitialMove (g:game) (pt1:point) (pt2:point) : bool = 
  suitableTown g pt1 && not (existsRoad g (g.gActive,(pt1,pt2))) 

(*Valid IFF colorOption is adjacent to piece and is not the active player*)
let validRobberMove (g:game) (piece:piece) (colorOption:color option) : bool = 
  List.mem colorOption (surroundingcolors g piece) 
  &&
  (*Ensure player does not select himself/herself*)
  (colorOption = None || get_some colorOption <> g.gActive) 

(*Valid IFF the *)
let validDiscardMove (g:game) (cost:cost) : bool = 
  let discardingPlayer = findPlayer g g.gNextColor in 
  validCost (minusCosts discardingPlayer.gPInventory cost)

let genMinInitialMove (g:game) : move = 
  let settlementPoint = settleablePoint g in 
  let roadLine = buildableRoad g settleablePoint in 
  InitialMove(settlementPoint,roadLine)

let genMinRobberMove (g:game) : move = 
  let piece = Random.int (cMAX_PIECE_NUM) in 
  let colorOption = pick_random (surroundingColorsNoOptions g piece) in 
  RobberMove(piece,colorOption) 

let genMinDiscardMove (g:game) : move = 
  let discardingPlayerColor = g.gNextColor in 
  let discardingPlayer =  findPlayer discardingPlayerColor


let surroundingColors (g:game) (piece:piece) : color option list = 
  let surroudningInters = piece_corners piece in 
  List.map (fun ele -> 
    let settlementOption = nthOfInterList g.gInterList ele in 
    if settlementOption = None 
    then None 
    else Some (fst(get_some settlementOption)) ) surroudningInters

let surroundingColorsNoOptions (g:game) (piece:piece) : color list = 
  let surroundingColorsOptions = surroundingColors g piece in 
  List.fold_left (fun acc ele -> if ele = None 
                                 then acc 
                                 else (get_some ele)::acc) 
                    [] surroundingColorsOptions
=======
>>>>>>> cd4bd8c5d258161f2f6593bb31063759c15f8cd3
                    
