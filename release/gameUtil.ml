(**********************************************************************)
(******              {Build related helper functions}            ******)
(**********************************************************************)

(* return list all possible adjacent roads with color equals to the
current active player *)
let all_adjacent_curColor_road (g:game) (pt:point) : road list = 
  mapList (fun adPt -> (g.gActive, (pt, adPt))) (adjacent_points pt)

(* helper function used to check whether a road already exists *)
let existsRoad (g:game) (road:road) : bool = 
  let (c, (p1, p2)) = road in
  let revRoad = (c, (p2, p1)) in
  not( List.mem road g.gRoadList) 
  && not (List.mem revRoad g.gRoadList)

(*Helper function to check if road is suitable. Need to consider the 
  whether the road has already been built(both (p1,p2)&&(p2, p1) order)
  and whether this new road is connect to a exsiting road belongs to the
  player, return false if road is not valid *)
let suitableRoad (g:game) (road:road) : bool = 
  let (c, (p1, p2)) = road in
  (* there are duplicates and road want to add in this 
    combined list, but doesn't matter *)
  let adjacentRoads = appendLists 
    (all_adjacent_curColor_road g p1) (all_adjacent_curColor_road g p2) in
  not (existsRoad g road) && existsList (existsRoad g) adjacentRoads

(* Helper function to check if town is valid, need to consider if there 
is not town already exists on that point and there are no town that is 
adjacent to it in one road length, return false if not valid *) 
let suitableTown (g:game) (town:point) : bool = 
  let interList = g.gInterList in
  (nthOfList interList town = None) && 
  (forAllList (fun p -> (nthOfList interList p) = None) 
    (adjacent_points town) )

(* Helper function used to check whether building a city on a point is
valid. Need to check that there is already a town on that point and
that the town belongs to the current active player, return false if not 
valid build site for city *)
let suitableCity (g:game) (city:point) : bool = 
  let interList = g.gInterList in
  match (nthOfList interList city) with
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
    let intersectionIndices = (mapiIntersecitons (fun i element -> i) g.gInterList) in 
    list_indexof (fun boolean -> boolean)
        (List.map (fun ele -> suitableSettlementPoint g ele) intersectionIndices) 

(*Helper function to check if road is already built*)
let suitableRoad (g:game) (road:road) : bool = 
    (*Return false if road is already built*)
    not( memRoadList road g.gRoadList) 

(*Function to return an unoccupied line with one end at pt *)
let buildableRoad (g:game) (pt:point) : line = 
    let occupiedLines = mapRoadList(fun (color,line) -> line) g.gRoadList in 
    let possibleRoads = List.map (fun ele -> (pt,ele) ) (adjacent_points pt) in
    List.find (fun ele -> (not) (List.mem ele occupiedLines)) possibleRoads

(**********************************************************************)
(******              {initial_move helper functions}             ******)
(**********************************************************************)

(*Count the total number of settlements (TOWNS AND CITIES) on the board*)
let countSettlements g = 
    list_count (fun ele -> if ele = None then false else true) g.gInterList

(*Return a list of indices of locations of this color's settlements*)
let findPlayerSettlements (g:game) (col:color) : point list =
  let interList = g.gInterList in
  let uncleanIndexList = mapiIntersecitons 
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


(* generate sources to all players and update game status *)
let generateResource (g : game) : game = 
  match g.gDiceRolled with
  | None -> g
  | Some roll -> 
    begin
      let hexList = g.gHexList in
      let interList = g.gInterList in
      let rec update hexList game index : game = 
        match hexList with 
        | [] -> game
        | hex::tl -> 
          begin
            let (ter, r) = hex in
            (* if current tile is not the rolled one, keep interating *)
            if r <> roll then
              update tl game (index+1)
            else 
            (* current hex number equals to roll number *)
              let curResource = resource_of_terrain ter in
              match curResource with
              (* if current resource is none, keep traversing the list *)
              | None -> update tl game (index+1)
              | Some res -> 
                begin
                  let curBaseRes = single_resource_cost res in  
                  let adjacentPoints = piece_corners index in
                  let adjacentInters = 
                    leftFoldList
                      (fun inters index -> 
                        (nthOfList interList index)::inters) 
                      [] adjacentPoints
                  in
                  
                  (* function used to handle adding resource of 
                  current intersection, and return new game status *)
                  let addResToPlayer (origGame:game) (inter:intersection)
                      : game = 
                    match inter with
                    | None -> origGame
                    (* update the game status when match occupied inter *)
                    | Some (color, settlement) ->
                      begin
                        let curAddRes = 
                          multiRes 
                            (settlement_num_resources settlement) curBaseRes 
                        in
                        let curPlayer = findPlayer origGame color in
                        let newInv = 
                          addCosts curAddRes curPlayer.gPInventory in
                        let newPlayer = 
                          {curPlayer with gPInventory = newInv} in
                        updatePlayer origGame newPlayer
                      end
                  in

                  (* interate all the adjacent inters to add 
                  resources to player occupied that inter and 
                  return new game status *)
                  let nextGame = 
                    leftFoldList addResToPlayer game adjacentInters 
                  in

                  update tl nextGame (index+1)
                end
          end
      in
      update hexList g 0
    end



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
        (interMatchPlayer (nthOfList interList p1) player) 
        || (interMatchPlayer (nthOfList interList p2) player)
      )) then ratio
    else cMARITIME_DEFAULT_RATIO
  in
  leftFoldList (fun acc e -> min acc (portRatio e))
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
(******              {BuyBuild helper functions}                ******)
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
      gRoadList = addToList road game.gRoadList;}

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
      gInterList = setNthList point newTown game.gInterList;}

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
      gInterList = setNthList point newCity game.gInterList;}

(* function used to build card and return a updatde game status *)
let buildCard (game:game) : game = 
  (* if deck is empty, then return original game status *)
  match game.gDeck with
  | Hidden _ -> game
  | Reveal cList -> 
      if(checkNull cList) then game
      else
        let (draw, remain) = pick_one cList in
        let curPlayer = findPlayer game game.gActive in
        let updatedPlayer = 
          {curPlayer with gPCard = (addToCard draw curPlayer.gPCard)} in
        let updatedPGame = updatePlayer game updatedPlayer in
        {updatedPGame with
          gDeck = Reveal remain;
          gCardsBought = addToCard draw updatedPGame.gCardsBought;
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

(* generate the next game status for next turn *)
let nextTurnGame (game:game) : game = 
  let resetGame = resetGameTurn game in 
  let nextPlayerColor = findNextPlayer game in
  {resetGame with
    gActive = nextPlayerColor;
    gNextColor = nextPlayerColor;
    gNextRequest = ActionRequest;
  }

(* function used to check whether there is a winner *)
let checkWinner (game:game) : color option = 
  failwith "checkWin unimplemented"
  