open Definition
open Constant
open Util
open Print
open GameUtil

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

let player_of_gPlayer (gp:gPlayer) : player = 
  (gp.gPColor, (gp.gPInventory, gp.gPCard), gp.gPTrophies)

let gPlayer_of_player (p:player) : gPlayer = 
  match p with
  | (color, (inventory, cards), trophies) ->
    {
      gPColor = color;
      gPInventory = inventory;
      gPCard = cards;
      gPTrophies = trophies;
    }

let state_of_game (g:game) : state =     
	(
		(*Board*)
		(
      (g.gHexList,g.gPortList),
      (g.gInterList,g.gRoadList),
      g.gDeck,
		  g.gDiscard, 
      g.gRobber
    ),

		(*Player List*)
		(List.map player_of_gPlayer g.gPlayerList),

		(*Turn *)
		{
      active         = g.gActive;
      dicerolled     = g.gDiceRolled;
      cardplayed     = g.gCardPlayed;
      cardsbought    = g.gCardsBought;
		  tradesmade     = g.gTradesMade;
      pendingtrade   = g.gPendingTrade;
    },

		(*Next*) 
		( 
      g.gNextColor, 
      g.gNextRequest
    )
	)            

let game_of_state (s:state) : game = 
  match s with 
	|(((hl,pl),(il,rl),dk,dc,rb),playerList,tn,(nc,nr))->

		(*Board*)
		{
      gHexList 	= hl;
		  gPortList 	= pl;
		  gInterList = il;
		  gRoadList 	= rl;
		  gDeck 		= dk;
		  gDiscard 	= dc; 
		  gRobber 	= rb; 
 
		  (*Player List*)
		  gPlayerList = (List.map gPlayer_of_player playerList);
 
		  (*Turn*)
		  gActive = tn.active;
		  gDiceRolled = tn.dicerolled;
		  gCardPlayed = tn.cardplayed;
		  gCardsBought = tn.cardsbought;
		  gTradesMade = tn.tradesmade;
		  gPendingTrade = tn.pendingtrade;
 
		  (*Next*)
		  gNextColor = nc;
		  gNextRequest = nr;
		}


let init_game () = game_of_state (gen_initial_state())



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


let handle_RobberMove g piece colorOption = (*STUB*)
let handle_DiscardMove g cost = (*STUB*)
let handle_TradeResponse g response = (*STUB*)


  
            

          




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
