open Definition
open Constant
open Util
open Print


type game = {
    gHexList                : hex list;
    gPortList               : port list;
    gInterList              : intersection list;
    gRoadList               : road list;
    gDeck                   : deck;
    gDiscard                : discard;
    gRobber                 : robber;
    gPlayerList             : player list;
    gActive                 : color;
    gDiceRolled             : roll option;
    gCardPlayed             : bool;
    gCardsBought            : cards;
    gTradesMade             : int;
    gPendingTrade           : trade option;
    gNextColor              : color;
    gNextRequest            : request;
}

let state_of_game g = failwith "A voice said look me in the stars"
let game_of_state s = failwith "And tell me truly, men of earth,"


let init_game () = game_of_state (gen_initial_state())


let handle_move s m = failwith "If all the soul-and-body scars"

let presentation s = failwith "Were not too much to pay for birth."
