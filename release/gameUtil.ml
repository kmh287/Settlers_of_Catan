(**********************************************************************)
(******                {general helper functions}                ******)
(**********************************************************************)

(*Function to return a new list with index i 
replaced with element e with initial list l*)
let updateList i e l = 
    List.mapi (fun index listelement ->
        if index = i then e 
        else listelement) l 

(*Helper function to check if pt && points adjacent to pt are unsettled*)
let suitableSettlementPoint g pt = 
    (*Return true if all points in the adjacency list are unsettled*)
    List.nth gInterList = None && 
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
    list_indexof suitableRoad possibleRoads


let addCosts cost1 cost2 = map_cost2 (+) cost1 cost2 


(* update game with a new player status *)
let updatePlayer game player = 
  let target = player.gPColor in
  let pList = game.gPlayerList in
  let newPList = List.map 
    (fun p -> if(p.gPColor = target) then player else p) pList in
  {game with gPlayerList = newPList}

(* find the player from game with specific color *)
let findPlayer game color = 
  let pList = game.gPlayerList in
  List.find (fun p -> p.gPColor = color) pList

(* add new resource to original invetory *)
let addInv newRes origRes : cost = 
  map_cost2 (fun n o -> n + o) newRes origRes

(* multiply res according to different type of settlement *)
let multiRes mul res : cost = 
  map_cost (fun r -> mul * r) res

(**********************************************************************)
(******              {initial_move helper functions}             ******)
(**********************************************************************)

(*Below are the helper functions for the various pieces of handle_move
None of these handle any specific move type, but rather make calculations
easier within the functions*) 

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
(******                {action helper functions}                 ******)
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
                    List.fold_left
                      (fun inters index -> 
                        (List.nth interList index)::inters) 
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
                          addInv curAddRes curPlayer.gPInventory in
                        let newPlayer = 
                          {curPlayer with gPInventory = newInv} in
                        updatePlayer origGame newPlayer
                      end
                  in

                  (* interate all the adjacent inters to add 
                  resources to player occupied that inter and 
                  return new game status *)
                  let nextGame = 
                    List.fold_left addResToPlayer game adjacentInters 
                  in

                  update tl nextGame (index+1)
                end
          end
      in
      update hexList g 0
    end
  