open Definition

type gPlayer = {
    gPColor                 : color;
    gPInventory             : inventory;
    gPCard                  : cards;
    gPKnights               : knights;
    gPLongestroad           : bool;
    gPLargestarmy           : bool;
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
      gHexList  = hl;
          gPortList     = pl;
          gInterList = il;
          gRoadList     = rl;
          gDeck         = dk;
          gDiscard  = dc; 
          gRobber   = rb; 
 
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