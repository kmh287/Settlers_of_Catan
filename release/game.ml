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

(*Function to return a new list with index i 
replaced with element e with initial list l*)
let updateList i e l = 
	List.mapi (fun index listelement ->
		if index = i then e 
		else listelement) l 

let handle_move s m =
	let g = game_of_state s in 
	match m with 
		|InitialMove( (pt1, pt2) ) -> if nr = InitialRequest
			(*If this move is of the expected type*) 
			g with gRoadList 	= (g.gActive,(pt1,pt2))::gRoadList;
			       gInterList 	=  updateList pt1 (g.gActive,Town) (g.gInterList)
			       gNextColor	= (*How do we advance this, do not know to go
			       					in forward or reverse order*)
				   gNextRequest = (*How can we determine whether the next request
				   					is an initial move or an action?*) 
			  
			else 
			let unoccupiedPt = list_indexof (fun ele ->
											if ele = None then true else false)
											g.gInterList in 
			let unoccupiedRoad = list_indexof (*Fill in later*)								
			g with gRoadList 	= 
				   gInterList 	= updateList unoccupiedPt (g.gActive,Town) (g.gInterList)
				   gNextColor	= (*See above*)
				   gNextRequest	= (*See above*)


let presentation s = failwith "Were not too much to pay for birth."
