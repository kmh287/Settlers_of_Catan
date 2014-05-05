open Definition
open Constant
open Util
open Print
include GameUtil

let player_of_gPlayer (gp:gPlayer) : player = 
  (gp.gPColor, (gp.gPInventory, gp.gPCard), 
      (gp.gPKnights, gp.gPLongestroad, gp.gPLargestarmy) )

let gPlayer_of_player (p:player) : gPlayer = 
  match p with
  | (color, (inventory, cards), (knights, lr, la) ) ->
    {
      gPColor           = color;
      gPInventory       = inventory;
      gPCard            = cards;
      gPKnights         = knights;
      gPLongestroad     = lr;
      gPLargestarmy     = la;
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


let handle_InitialMove (g:game) (pt1:point) (pt2:point) : game = 
  (*Num settlements INCLUDING one about to be placed*)
  let settlementNum = countSettlements g +1 in    
  (*Point to use if pt1 is an invalid settle spot*)
	(*Return updated record*)
 	{g with 	gInterList = setNthInterList pt1 (Some(g.gActive,Town)) (g.gInterList); 

   	   			gRoadList  = (g.gActive,(pt1,pt2))::g.gRoadList;

         		gPlayerList=(*Only add resources after fifth settlement 
			                 	is placed*)
				                if settlementNum <= 4 
				                then g.gPlayerList
				                else setNthPlayerList 
				                  (*Index*)
				                  (findPlayerIndex g g.gActive)
				                  (*Updated value*)
				                  (initUpdateResources g (g.gActive))
				                  (*List*)
				                  (g.gPlayerList); 

          	gNextColor= (*Travel forward during first half of iniital phase
			              		and at the very end *)
						            if (settlementNum < 4 || settlementNum >= 8)
						            then next_turn g.gActive 
						            (*If already four settlements, go in reverse*)
						            else prev_turn g.gActive;  

    				gActive  	= (*Travel forward during first half of iniital phase
						            and at the very end *)
						            if (settlementNum < 4 || settlementNum >= 8)
						            then next_turn g.gActive 
						            (*If already four settlements, go in reverse*)
						            else prev_turn g.gActive;  

	    		gNextRequest= if settlementNum >= 8 
					              then ActionRequest
					              (*If fewer than 8 settlements, then still init
					              phase*)
					              else InitialRequest;}


(*Handle the robber move, the piece and color option should already have been
scrubbed by the scrubber, and are thus assumed to be correct.*)
let handle_RobberMove (g:game) (piece:piece) (colorOption:color option) (knight:bool): game = 
  if colorOption = None || knight 
  then {
          g with gRobber      = piece;
                 gNextRequest = ActionRequest;
        }
  else  
        let discardColor = get_some colorOption in 
        {
          g with gRobber      = piece;
                 gNextRequest = ActionRequest;
                 gNextColor   = g.gActive
        }

(*Handle discards due to the robber. A resource is selected at random 
and one unit of it is removed from the inventory. The cost should have already been
scrubbed, and is thus assumed to be correct*)
let handle_DiscardMove (g:game) (cost:cost) : game = 
  (*Discarding player is still set as next color*)
  let discardingColor       = g.gNextColor in 
  let discardingPlayer      = findPlayer g discardingColor in 
  let discardingPlayerInv   = discardingPlayer.gPInventory in 

  {
  g with gPlayerList         = setNthPlayerList 
                              (*Index*)
                              (findPlayerIndex g discardingColor) 
                              (*Updated value*)
                              {discardingPlayer with 
                                gPInventory = minusCosts discardingPlayerInv cost;}
                              (*List*)
                              (g.gPlayerList); 
          gNextColor        = next_color (g.gNextColor); 
          gNextRequest      = if next_color (g.gNextColor) = g.gActive 
                              then ActionRequest
                              else DiscardRequest; 
  } 

let handle_TradeResponse (game:game) (response:bool) : game = 
  (* if trade request has been rejected or 
    current pending trade is not valid *)
  if(not response || game.gPendingTrade = None) 
    then {game with 
      gPendingTrade = None;
      gNextRequest = ActionRequest;
      gNextColor = game.gActive;
    }
  (* if trade is success *)
  else 
    match game.gPendingTrade with
    | None -> {game with 
      gPendingTrade = None;
      gNextRequest = ActionRequest;
      gNextColor = game.gActive;
    }
    | Some (tColor, sell, buy) -> 
      begin
        let curPlayer = findPlayer game game.gActive in
        let tradePlayer = findPlayer game tColor in
        let updatedCurPlayer = {curPlayer with
          gPInventory = 
            (addCosts (minusCosts curPlayer.gPInventory sell) buy);
        } in
        let updatedTradePlayer = {tradePlayer with
          gPInventory = 
            (addCosts (minusCosts tradePlayer.gPInventory buy) sell);
        } in
        let updatedPGame = (updatePlayer
          (updatePlayer game updatedCurPlayer) updatedTradePlayer) in
        {updatedPGame with
          gPendingTrade = None;
          gNextRequest = ActionRequest;
          gNextColor = game.gActive;
        }
      end

let handle_Action (game:game) (action:action) : game outcome = 
  match action with
  | RollDice -> 
    let rolledGame = {game with 
      gDiceRolled = Some (random_roll ())} in
    if rolledGame.gDiceRolled = Some cROBBER_ROLL then
      (None, {rolledGame with 
        gNextRequest = DiscardRequest;
        gNextColor = (next_turn game.gActive);
      })
    else
      let updatedGame = generateResource rolledGame in
      (None, {updatedGame with gNextColor = updatedGame.gActive})
  | MaritimeTrade mtrade -> 
      let curPlayer = findPlayer game game.gActive in
      let (sell, buy) = mtrade in
      let ratio = getMariTradeRatio game sell game.gActive in
      let origInv = curPlayer.gPInventory in
      let updatedInv = updateInventory sell buy ratio origInv in
      let updatedPlayer = {curPlayer with gPInventory = updatedInv;} in
      let updatedGame = updatePlayer game updatedPlayer in
      (None, {updatedGame with gNextColor = game.gActive;})
  | DomesticTrade trade -> 
      let (tradeColor, _, _) = trade in
      (None, {game with

        (* do we need to check number of trades made in this fuction? *)
        gTradesMade = game.gTradesMade + 1;

        gNextColor = tradeColor;
        gNextRequest = TradeRequest;
        gPendingTrade = Some trade;
      })
  | BuyBuild build -> 
    let newGame = 
      (match build with
      | BuildRoad road -> buildRoad game road
      | BuildTown point -> buildTown game point
      | BuildCity point -> buildCity game point
      | BuildCard -> buildCard game)
    in (None, newGame)
  | PlayCard playcard -> 
    begin
      let curPlayer = findPlayer game game.gActive in
      let uPlayer = 
        removeCardFromPlayer curPlayer (cardOfPlaycard playcard) in
      let game = updatePlayer game uPlayer in
      match playcard with
      | PlayKnight (piece, colorOp) ->
          let curPlayer = findPlayer game game.gActive in
          let updatedPlayer = 
            {curPlayer with gPKnights = curPlayer.gPKnights + 1} in
          let updatedPGame = updatePlayer game updatedPlayer in
          let updatedRGame = 
            handle_RobberMove updatedPGame piece colorOp true in
          (None, {updatedRGame with 
            gCardPlayed = true;
          })
      | PlayRoadBuilding (road1, roadOption) -> 
          let buildOneGame = buildRoad game road1 in
          let buildTwoGame = (match roadOption with
            | None -> buildOneGame
            | Some road2 -> buildRoad game road2
          ) in
          (None, {buildTwoGame with
            gCardPlayed = true;
            gNextRequest = ActionRequest;
            gNextColor = game.gActive;
          })
      | PlayYearOfPlenty (res, resOption) ->
          let curPlayer = (findPlayer game game.gActive) in
          let inv = curPlayer.gPInventory in
          let incRes1Inv = increaseResInInventory inv res 1 in
          let incRes2Inv = (match resOption with
            | None -> incRes1Inv
            | Some res2 -> increaseResInInventory inv res2 1
          ) in
          let updatedPlayer = {curPlayer with gPInventory = incRes2Inv;} in
          let updatedPGame = updatePlayer game updatedPlayer in
          (None, {updatedPGame with
            gCardPlayed = true;
            gNextRequest = ActionRequest;
            gNextColor = game.gActive;
          })
      | PlayMonopoly res ->
          let sumRes = leftFoldPlayerList (fun sum p -> 
            sum + num_resource_in_inventory p.gPInventory res
          ) 0 game.gPlayerList in
          let curColor = game.gActive in
          let updatedPlayerList = mapPlayerList (fun p ->
            if(p.gPColor = curColor) then setPlayerInvRes p res sumRes
            else setPlayerInvRes p res 0
          ) game.gPlayerList
          in
          (None, {game with
            gCardPlayed = true;
            gPlayerList = updatedPlayerList;
            gNextRequest = ActionRequest;
            gNextColor = game.gActive;
          })
    end
  | EndTurn ->
      let trophyGame = checkTrophies game in 
      let winner = checkWinner trophyGame in
      (winner, nextTurnGame trophyGame)



let handle_move (g:game) (m:move) : game outcome =
  match scrubMove g m with 
    |InitialMove( (pt1, pt2) ) -> 
      (None, handle_InitialMove g pt1 pt2)
    |RobberMove (piece,colorOption) -> 
      (None, handle_RobberMove g piece colorOption false)
    |DiscardMove(cost) -> 
      (None, handle_DiscardMove g cost)
    |TradeResponse(response) ->
      (None, handle_TradeResponse g response)
    |Action(action) ->
      handle_Action g action


(*Return a state with all the opponents' cards obscured*)
let presentation (g:game) : game =  
  let currentPlayerColor = g.gActive in
  (*Map over player list, hide cards of all other playerS*)
  let newPlayerList = 
    mapPlayerList (fun player ->  
      if (getPlayerColor player = currentPlayerColor)
        then player
      else hidePlayerCards player) g.gPlayerList in 
    {g with gPlayerList = newPlayerList;}
