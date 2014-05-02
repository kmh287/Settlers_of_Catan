(* open Definition
open Constant
open Util
open Print
open GameUtil

 *)

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





(*************helper functions for each case of handle_move.**********
****************They are divided into each case***********************
*****Each function here will take a game and relevent parameters******
*****and return a game. The conversion to state will happen in *******
**************************handle_move*********************************)


let handle_InitialMove g pt1 pt2 = 
  failwith "type incorrected"
  (*
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
          gRoadList   = if suitableSettlementPoint g pt1 
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

            gNextColor  = (*Travel forward during first half of iniital phase
                  and at the very end *)
                  if (settlemenNum < 4 || settlementNum >= 8)
                  then next_turn g.gActive 
                  (*If already four settlements, go in reverse*)
                  else prev_turn g.gActive;  

          gActive   = (*Travel forward during first half of iniital phase
                  and at the very end *)
                  if (settlemenNum < 4 || settlementNum >= 8)
                  then next_turn g.gActive 
                  If already four settlements, go in reverse
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
    let unoccupiedPt  = settleablePoint () in 
    let unoccupiedRoad  = buildableRoad unoccupiedPt    
    let settlementNum = countSettlements ()+1 in    

    (*Return updated record*)
    g with  gInterList  = updateList unoccupiedPt 
                        (g.gActive,Town) 
                        (g.gInterList);

        gRoadList   = (g.gActive,unoccupiedRoad)::g.gRoadList;

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

         gNextColor = (*Travel forward during first half of iniital phase
                  and at the very end *)
                  if (settlemenNum < 4 || settlementNum >= 8)
                  then next_turn g.gActive 
                  (*If already four settlements, go in reverse*)
                  else prev_turn g.gActive;  

         gActive    = (*Travel forward during first half of iniital phase
                  and at the very end *)
                  if (settlemenNum < 4 || settlementNum >= 8)
                  then next_turn g.gActive 
                  (*If already four settlements, go in reverse*)
                  else prev_turn g.gActive;  

         gNextRequest = if settlementNum >= 8 
                  then ActionRequest
                  (*If fewer than 8 settlements, then still init
                  phase*)
                  else InitialRequest
      end 
*)


let handle_RobberMove g piece colorOption = 
  failwith "handle_RobberMove unimplemented"
let handle_DiscardMove g cost = 
  failwith "handle_DiscardMove unimplemented"
let handle_TradeResponse g response = 
  failwith "handle_TradeResponse unimplemented"



let handle_Action (game:game) (action:action) : game = 
  match action with
  | RollDice -> 
    let rolledGame = {game with 
      gDiceRolled = Some (random_roll ())} in
    if rolledGame.gDiceRolled = Some cROBBER_ROLL then
      {rolledGame with 
        gNextRequest = RobberRequest;
        gNextColor = game.gActive;
      }
    else
      let updatedGame = generateResource rolledGame in
      {updatedGame with gNextColor = updatedGame.gActive}
  | MaritimeTrade mtrade -> 
      let curPlayer = findPlayer game game.gActive in
      let (sell, buy) = mtrade in
      let ratio = getMariTradeRatio game sell game.gActive in
      let origInv = curPlayer.gPInventory in
      let updatedInv = updateInventory sell buy ratio origInv in
      let updatedPlayer = {curPlayer with gPInventory = updatedInv;} in
      let updatedGame = updatePlayer game updatedPlayer in
      {updatedGame with gNextColor = game.gActive;}
  | DomesticTrade trade -> 
      let (tradeColor, _, _) = trade in
      {game with

        (* do we need to check number of trades made in this fuction? *)
        gTradesMade = game.gTradesMade + 1;

        gNextColor = tradeColor;
        gNextRequest = TradeRequest;
        gPendingTrade = Some trade;
      }
  | BuyBuild build -> 
    (match build with
    | BuildRoad road -> buildRoad game road
    | BuildTown point -> buildTown game point
    | BuildCity point -> buildCity game point
    | BuildCard -> buildCard game)
  | PlayCard playcard -> failwith "unimplemented"
  | EndTurn ->
      (* let winner = checkWinner game in *)
      nextTurnGame game


let handle_move g m =
  match m with 
    |InitialMove( (pt1, pt2) ) -> 
        state_of_game (handle_InitialMove g pt1 pt2)  
    |RobberMove ( (piece,colorOption) ) -> 
        state_of_game (handle_RobberMove g piece colorOption)
    |DiscardMove(cost) -> 
        state_of_game (handle_DiscardMove g cost)
    |TradeResponse(response) ->
        state_of_game (handle_TradeResponse g response)

    (*This final case MAY require its own function to match the action*)
    (*MAYBE we should match on the action here*)
    |Action(action) ->
        state_of_game (handle_Action g action)



let presentation s = failwith "Were not too much to pay for birth."
