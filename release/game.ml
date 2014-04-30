open Definition
open Constant
open Util
open Print


(* add a gPlayer type to represent player *)
type gPlayer = {
    gPColor                 : color;
    gPInventory             : inventory;
    gPCard                  : cards;
    gPTrophies              : trophies;
}

type game = {
	(*Board*)
    gHexList                : hex list;
    gPortList               : port list;
    gInterList              : intersection list;
    gRoadList               : road list;
    gDeck                   : deck;
    gDiscard                : discard;
    gRobber                 : robber;

    (*Player list*)
    gPlayerList             : gPlayer list;

    (*Turn*)
    gActive                 : color;
    gDiceRolled             : roll option;
    gCardPlayed             : bool;
    gCardsBought            : cards;
    gTradesMade             : int;
    gPendingTrade           : trade option;

    (*Next*)
    gNextColor              : color;
    gNextRequest            : request;
}

let state_of_game g =     
		(
		(*Board*)
		(g.gHexList,g.gPortList,g.gInterList,g.gRoadList,g.gDeck,
		g.gDiscard, g.gRobber),

		(*Player List*)
		gPlayerList,

		(*Turn *)
		(g.gActive,g.gDiceRolled,g.gCardPlayed,g.gCardsBought
		g.gTradesMade, g.gPendingTrade),

		(*Next*) 
		(g.gNextColor, g.gNextRequest)
		)              

let game_of_state s = match s with 
	|((hl,pl,il,rl,dk,dc,rb),playerList,(a,dr,cp,cb,tm,pt),(nc,nr))->
		
		(*Board*)
		{gHexList 	= hl;
		 gPortList 	= pl;
		 gInterList = il;
		 gRoadList 	= rl;
		 gDeck 		= dk;
		 gDiscard 	= dc; 
		 gRobber 	= rb; 

		 (*Player List*)
		 gPlayerList = playerList;

		 (*Turn*)
		 gActive = a;
		 gDiceRolled = dr;
		 gCardPlayed = cp;
		 gCardsBought = cb;
		 gTradesMade = tm;
		 gPendingTrade = pt;

		 (*Next*)
		 gNextColor = nc;
		 gNextRequest = nr;
		 }
	|_ -> failwith "Implementation fault"



let init_game () = game_of_state (gen_initial_state())

(*Below are the helper functions for the various pieces of handle_move
None of these handle any specific move type, but rather make calculations
easier within the functions*) 

let countSettlements g = 
	list_count (fun ele -> if ele = None then false else true) g.gInterList

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

(*Function to search for a point that 
does not have have a settle men nor
adjacent settlements*)
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

(*Resource updater FOR INIT PHASE*)
let initUpdateResources g color : player = 
	(*Find the index of the player in the player list*)
	let index = list_indexof (fun ele -> fst(ele) = color) in 
	(*Map over the list, turn all points that don't belong to color to None*)
	let indexList = List.mapi (fun index ele -> if fst(ele) = color 
											   then index 
											   else -1) in 
	(*Map over index list, return a list with only the indicies and no None*)
	let indexList = List.fold_left (fun acc ele -> if ele <> -1
												   then ele::acc
												   else acc) [] g.gInterList in
	(*Find all pieces adjacent to the indices in indexList*)
	let pieceList = List.flatten (List.map (fun ele ->
										 adjacent_pieces ele) indexList) in
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
	match List.nth g.gPlayerList index with 
		|(col,hand,t) -> match hand with
			(*Return a player with the proper resources added*)
			|(inv,cds) -> (col,(addCosts inv totalNewResources,cds),t) 

let handle_move g m =
	match m with 
		|InitialMove( (pt1, pt2) ) -> 
				state_of_game (handle_InitialMove g pt1 pt2)  
		|RobberMove ( (piece,colorOption) ) -> 
				state_of_game (handle_RobberMove g piece colorOption)
		|Discard(cost) -> 
				state_of_game (handle_DiscardMove g cost)
		|TradeResponse(response) ->
				state_of_game (handle_TradeResponse g response)

		(*This final case MAY require its own function to match the action*)
		(*MAYBE we should match on the action here*)
		|Action(action) ->
				state_of_game (handle_Action g action)

(*************helper functions for each case of handle_move.**********
****************They are divided into each case***********************
*****Each function here will take a game and relevent parameters******
*****and return a game. The conversion to state will happen in *******
**************************handle_move*********************************)

let handle_InitialMove g pt1 pt2 = 
	(*If nextRequest is initial move, then handle appropriately*)
	if g.gNextRequest = InitialMove then begin 
		(*Num settlements INCLUDING one about to be placed*)
		let settlementNum = countSettlements g +1 in 		
		(*Point to use if pt1 is an invalid settle spot*)
		let settlePoint = settleablePoint g  in

		(*Return updated record*)
	     g with gInterList  = (*Check if provided pt1 is valid to settle*)
	       					   if suitableSettlementPoint g pt1 
	       					   then updateList pt1 (g.gActive,Town) (g.gInterList)
	       					   else updateList ( settlePoint ) 
	       					   					(g.gActive,Town) 
	       					   					(g.gInterList);

	       					   (*Check if provided pt1 is valid to settle
	       					   if it isn't, then the road is invalid too*)
		   		gRoadList 	= if suitableSettlementPoint g pt1 
							  then (g.gActive,(pt1,pt2))::g.gRoadList;
							  else (g.gActive,(settlePoint,
							  			(buildableRoad g settlePoint)::g.gRoadList;

	      	 	gPlayerList = (*Only add resources after fifth settlement 
	       					   is placed*)
	       					  if settlemenNum <= 4 
	       					  then g.gPlayerList
	       					  else updateList 
		       					  (*Index*)
		       					  list_indexof (fun ele -> fst(ele) = g.gActive)
		       					  (*Updated value*)
		       					  initUpdateResources g (g.gActive)
		       					  (*List*)
		       					  g.gPlayerList; 

	       		gNextColor	= (*Travel forward during first half of iniital phase
			   				  and at the very end *)
			   				  if (settlemenNum < 4 || settlementNum >= 8)
			   				  then next_turn g.gActive 
			   				  (*If already four settlements, go in reverse*)
			   				  else prev_turn g.gActive;  

		   		gActive 	= (*Travel forward during first half of iniital phase
			   				  and at the very end *)
			   				  if (settlemenNum < 4 || settlementNum >= 8)
			   				  then next_turn g.gActive 
			   				  (*If already four settlements, go in reverse*)
			   				  else prev_turn g.gActive;  

		   		gNextRequest= if settlementNum >= 8 
			   				  then ActionRequest
			   				  (*If fewer than 8 settlements, then still init
			   				  phase*)
			   				  else InitialRequest
	  end 

	  (*If nextRequest is not an initial move, then enter a minimal 
	  move. This move will find the first unoccupied point and settle it*)
	else begin 
		let unoccupiedPt 	= settleablePoint () in 
		let unoccupiedRoad	= buildableRoad unoccupiedPt 		
		let settlementNum = countSettlements ()+1 in 		

		(*Return updated record*)
		g with  gInterList 	= updateList unoccupiedPt 
			   								(g.gActive,Town) 
			   								(g.gInterList);

				gRoadList 	= (g.gActive,unoccupiedRoad)::g.gRoadList;

	         	gPlayerList = (*Only add resources after fifth settlement 
	       					   is placed*)
	       					  if settlemenNum <= 4 
	       					  then g.gPlayerList
	       					  else updateList 
		       					  (*Index*)
		       					  list_indexof (fun ele -> fst(ele) = g.gActive)
		       					  (*Updated value*)
		       					  initUpdateResources (g.gActive)
		       					  (*List*)
		       					  g.gPlayerList; 

			   gNextColor	= (*Travel forward during first half of iniital phase
			   				  and at the very end *)
			   				  if (settlemenNum < 4 || settlementNum >= 8)
			   				  then next_turn g.gActive 
			   				  (*If already four settlements, go in reverse*)
			   				  else prev_turn g.gActive;  

			   gActive 		= (*Travel forward during first half of iniital phase
			   				  and at the very end *)
			   				  if (settlemenNum < 4 || settlementNum >= 8)
			   				  then next_turn g.gActive 
			   				  (*If already four settlements, go in reverse*)
			   				  else prev_turn g.gActive;  

			   gNextRequest	= if settlementNum >= 8 
			   				  then ActionRequest
			   				  (*If fewer than 8 settlements, then still init
			   				  phase*)
			   				  else InitialRequest
			end 

let handle_RobberMove g piece colorOption = (*STUB*)
let handle_DiscardMove g cost = (*STUB*)
let handle_TradeResponse g response = (*STUB*)


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
  
  
            

          




let handle_Action g action = 
  match action with
  | RollDice -> 
    let rolledGame = {g with gDiceRolled = Some (random_roll ())} in
    if rolledGame.gDiceRolled = cROBBER_ROLL then
      {rolledGame with 
        rolledGame.gNextRequest = RobberRequest;
        rolledGame.gNextColor = g.gActive;
      }
    else
      generateResource rolledGame

  | MaritimeTrade mtrade ->

  | DomesticTrade trade ->

  | BuyBuild build ->

  | PlayCard playcard ->

  | EndTurn ->





let presentation s = failwith "Were not too much to pay for birth."
