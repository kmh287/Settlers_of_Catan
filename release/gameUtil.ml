(**********************************************************************)
(******            {Model Related helper functions}              ******)
(**********************************************************************)

(* function used to fold_left on a list *)
let leftFoldList = List.fold_left

(* return the nth element of lst *)
let nthOfList = List.nth

(* return the index of first element satisfy the predicate *)
let indexOfList = list_indexof 

let sizeOfList = List.length

let mapList = List.map

let filterOnList = List.filter

let forAllList = List.for_all

let addToList ele lst = [ele]@lst 

(* function used to set nth of list to a new element, if
n is greated than the list size, than return original list *)
let setNthList (n:int) (ele:'a) (lst:'a list) : 'a list = 
  List.mapi (fun index e -> if (index = n) then ele else e) lst

(*Function to return a new list with index i 
replaced with element e with initial list l*)
let updateList i e l = 
    List.mapi (fun index listelement ->
        if index = i then e 
        else listelement) l 


(**********************************************************************)
(******            {Player related helper functions}             ******)
(**********************************************************************)

(* update game with a new player status *)
let updatePlayer (game:game) (player:gPlayer) : game = 
  let target = player.gPColor in
  let pList = game.gPlayerList in
  let newPList = List.map 
    (fun p -> if(p.gPColor = target) then player else p) pList in
  {game with gPlayerList = newPList}

(* find the player from game with specific color *)
let findPlayer game color = 
  let pList = game.gPlayerList in
  List.find (fun p -> p.gPColor = color) pList


(* match the intersection with specific color *)
let interMatchPlayer (inter:intersection) (player:color) : bool = 
  match inter with
  | None -> false
  | Some(c, _) -> c = player


(**********************************************************************)
(******     {Resource and Inventory related helper functions}    ******)
(**********************************************************************)

(* sum up two cost accordingly *)
let addCosts (cost1:cost) (cost2:cost) : cost = 
  map_cost2 (+) cost1 cost2 

(* cost1 - cost2 *)
let minusCosts (cost1:cost) (cost2:cost) : cost = 
  map_cost2 (-) cost1 cost2

(* multiply res according to different type of settlement *)
let multiRes mul res : cost = map_cost ( ( * ) mul ) res

(* update the inventory with specific resource and delta,
delta can be negtive as well, need to check before call *)
let increaseResInInventory 
    (inv:inventory) (res:resource) (delta:int) : inventory = 
  let (b,w,o,l,g) = inv in
  match res with
  | Brick  ->    (b + delta, w, o, l, g)
  | Wool   ->    (b, w + delta, o, l, g)
  | Ore    ->    (b, w, o + delta, l, g)
  | Grain  ->    (b, w, o, l + delta, g)
  | Lumber ->    (b, w, o, l, g + delta)


(* compare cost1 with cost2, return true only if every subset in
  cost1 is greater than or equal to every subset accordingly in cost2 *)
let greaterThanEqual (cost1:cost) (cost2:cost) : bool = 
  let (bi,wi,oi,li,gi) = cost1 and (b,w,o,l,g) = cost2 in 
  bi >= b && wi >= w && oi >=o && li >= l && gi >= g 

(* compare cost1 with cost2, return true only if every subset in
  cost1 is greater than every subset accordingly in cost2 *)
let lessThanEqual (cost1:cost) (cost2:cost) : bool = 
  let (bi,wi,oi,li,gi) = cost1 and (b,w,o,l,g) = cost2 in 
  bi <= b && wi <= w && oi <=o && li <= l && gi <= g 


(**********************************************************************)
(******              {Build related helper functions}            ******)
(**********************************************************************)

(*Helper function to check if road is already built, need to consider
points are in a reversed order, return false if road is already built*)
let suitableRoad (g:game) (road:road) : bool = 
  let (c, (p1, p2)) = road in
  let revRoad = (c, (p2, p1)) in
  not( List.mem road g.gRoadList) && not (List.mem revRoad g.gRoadList)

(* Helper function to check if town is valid, need to consider if there 
is not town already exists on that point and there are no town that is 
adjacent to it in one road length, return false if not valid *) 
let suitableTown (g:game) (town:point) : bool = 
  let interList = g.gInterList in
  (nthOfList interList town = None) && 
  (forAllList (fun p -> (nthOfList interList p) = None) 
    (adjacent_points town) )

(* Helper function used to check whether building a city on a point is
valid. Need to consider that there is already a town on that point and
the town is belong to the current active player, return false is not 
valid *)
let suitableCity (g:game) (city:point) : bool = 
  let interList = g.gInterList in
  match (nthOfList interList city) with
  | Some (c, settlement) -> c = g.gActive && settlement = Town
  | _ -> false

(* function that are not type correct, need to be fixed

(*Helper function to check if pt && points adjacent to pt are unsettled*)
let suitableSettlementPoint g pt = 
    (*Return true if all points in the adjacency list are unsettled*)
    List.nth g.gInterList = None && 
    (List.for_all (fun ele -> List.nth g.gInterList ele = None) 
                    adjacent_points pt) in 

(*Function to search for a point that does not have 
have a settlement nor adjacent settlements*)
let settleablePoint g : point= 

    (*Find index of first element that is settleable in interlist*)
    list_indexof suitableSettlementPoint g.gInterList

(*Helper function to check if road is already built*)
let suitableRoad g road = 
    (*Return false if road is already built*)
    not( List.mem road g.gRoadList) 

(*Function to return a buildable road adjacent to pt *)
let buildableRoad g pt : road = 
    let possibleRoads = List.map (fun ele -> (pt,ele) ) (adjacent_points pt) in
    list_indexof (suitableRoad g possibleRoads)

*)

(**********************************************************************)
(******              {initial_move helper functions}             ******)
(**********************************************************************)

(*

let countSettlements g = 
    list_count (fun ele -> if ele = None then false else true) g.gInterList


(*Resource updater FOR INIT PHASE*)
let initUpdateResources g color : player = 
    (*Find the index of the player in the player list*)
    let index = list_indexof (fun ele -> fst(ele) = color) g.gInterList in 
    (*Map over the list, turn all points that don't belong to color to -1*)
    let indexList = List.mapi (fun index ele -> if fst(ele) = color 
                                               then index 
                                               else -1) in 
    (*Map over index list, return a list with only the indicies and no -1*)
    let cleanedIndexList = List.fold_left (fun acc ele -> if ele <> -1
                                                   then ele::acc
                                                   else acc) [] indexList in
    (*Find all pieces adjacent to the indices in indexList*)
    let pieceList = List.flatten (List.map (fun ele ->
                                         adjacent_pieces ele) cleanedIndexList) in
    (*Lookup piece numbers in hex list, and build list of terrain types*)
    let terrainList = List.map (fun ele -> 
                                    fst(List.nth g.gHexList ele)) pieceList in 
    let rec resourceGatherer (tlist:terain list) (acc:cost) : cost = 
        match tlist with 
            |[] -> acc
            |hd::tl -> let rsource = resource_of_terrain hd  in 
                       if rsource = None 
                       (*No resource, then move down the list *)
                       then resourceGatherer tl acc
                       (*If there is a resource, add the cost to the acc*)
                       else let resourceTotal =
                         map_cost (fun ele -> ele * cRESOURCES_GENERATED_TOWN)
                                  single_resource_cost rsource in  
                       (*Call again on tail, with acc + new resources*)
                       resourceGatherer tl 
                                    addCosts acc resourceTotal  in 
    let totalNewResources = resourceGatherer terrainList (0,0,0,0,0) in 

    (*Deconstruct player list, then build it again with new resources added*)
    match (List.nth g.gPlayerList index) with 
        |(col,hand,t) -> match hand with
            (*Return a player with the proper resources added*)
            |(inv,cds) -> (col,(addCosts inv totalNewResources,cds),t) 

*)


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
                  let addResToPlayer origGame inter : game = 
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
  failwith "buildGame unimplemented"  

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
  