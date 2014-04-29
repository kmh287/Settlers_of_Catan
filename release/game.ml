open Definition
open Constant
open Util
open Print


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
    gPlayerList             : player list;

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

(*Function to return a new list with index i 
replaced with element e with initial list l*)
let updateList i e l = 
	List.mapi (fun index listelement ->
		if index = i then e 
		else listelement) l 

(*Function to search for a point that 
does not have have a settle men nor
adjacent settlements*)
let settleablePoint () = 

	(*Helper function to check if pt && points adjacent to pt are unsettled*)
	let suitableSettlementPoint pt = 
		(*Return true if all points in the adjacency list are unsettled*)
		List.nth gInterList = None && 
		(List.for_all (fun ele -> List.nth g.gInterList ele = None) 
						adjacent_points pt) in 
		
	(*Find index of first element that is settleable in interlist*)
	list_indexof suitableSettlementPoint g.gInterList


let handle_move s m =
	let g = game_of_state s in 
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
		g with gRoadList 	= (g.gActive,(pt1,pt2))::gRoadList;
	       gInterList 	=  updateList pt1 (g.gActive,Town) (g.gInterList)
	       gNextColor	= (*How do we advance this, do not know to go
	       					in forward or reverse order*)
		   gNextRequest = (*How can we determine whether the next request
		   					is an initial move or an action?*) 
	  end 

	  (*If nextRequest is not an initial move, then enter a minimal 
	  move. This move will find the first unoccupied point and settle it*)
	else begin 
		let unoccupiedPt = list_indexof (fun ele ->
										if ele = None then true else false)
										g.gInterList in 
		let unoccupiedRoad = list_indexof (*Fill in later*)								
		g with gRoadList 	= 
			   gInterList 	= updateList unoccupiedPt (g.gActive,Town) (g.gInterList)
			   gNextColor	= (*See above*)
			   gNextRequest	= (*See above*)
			end 

let handle_RobberMove g piece colorOption = (*STUB*)
let handle_DiscardMove g cost = (*STUB*)
let handle_TradeResponse g response = (*STUB*)
let handle_Action g action = (*STUB*)



let presentation s = failwith "Were not too much to pay for birth."
