(******************************************************************************)
(** {2 Game Representation}                                                   *)
(******************************************************************************)


(** This is the forward cyclic order of player turns *)
type color = Blue | Red | Orange | White

(** The six terrain types on the board hexes. Each produces a
    corresponding resource when the hex's dice number is rolled. *)
type terrain = Hill | Pasture | Mountain | Field | Forest | Desert

(** The resource types produced by the above terrains, respectively.
    Desert does not produce any resource *)
type resource = Brick | Wool | Ore | Grain | Lumber

(** The types of development cards available in the game. *)
type card = Knight | VictoryPoint | RoadBuilding | YearOfPlenty | Monopoly

(** Dice roll values. Valid for 2 - 12 inclusive. *)
type roll = int
(** Hex list index. Valid for 0-18 inclusive. *)
type piece = int
(** Point list index. Valid for 0 - 53 inclusive. *)
type point = int
(** Maritime trade ratio. Valid for 2, 3, or 4 *)
type ratio = int

(** Valid for two adjacent points. [(p1, p2)] is line-equal to [(p2, p1)] *)
type line = point * point
(** One tile on the map. roll is the dice number that will produce resources *)
type hex = terrain * roll

(** In resource enumerated order (B,W,O,G,L) *)
type cost = int * int * int * int * int
(** Number of each resource a player has available in B,W,O,G,L order *)
type inventory = cost

(** All card lists in the game are encoded in this type so that 
    players cannot look at cards they shouldn't be able to access *)
type cards = Hidden of int | Reveal of card list
(** Represents a player's hand *)
type hand = inventory * cards


(******************************************************************************)
(** {2 TROPHIES: properties of individual players}                            *)
(******************************************************************************)

(** Number of knights the player has played thus far *)
type knights = int
(** True if the player currently has the longest road *)
type longestroad = bool
(** True if the player currently has the largest army (most knights played) *)
type largestarmy = bool
(** The three trophies the player can own (property of an individ. player *)
type trophies = knights * longestroad * largestarmy

(** Player's color, cards they own, and trophies they have. One of four "big"
    components in the overall game state.*)
type player = color * hand * trophies


(** Ports can either offer a trade ratio for a specific resource,
    or can offer a generic ratio for any resource *)
type portresource = Any | PortResource of resource
(** A port provides a specific trade ratio (e.g. ratio 4 means a 4:1 trade)
    and services the two points along its line. *)
type port = line * ratio * portresource

(** Settlement type. *)
type settlement = Town | City

(** [Some (c,s)] if the settlement [s] of color [c] exists on the board.
    Each intersection is indexed by a point *)
type intersection = (color * settlement) option
(** A road built by a player of a specific color,
    across a specific line on the board *)
type road = color * line

(** The static map: 19 tiles, 9 ports standard. Lists are in the indexed order
    given by the game board in the writeup. *)
type map = hex list * port list
(** The variable board: 53 intersections standard, 0 initial roads *)
type structures = intersection list * road list
(** Deck of cards not yet drawn. *)
type deck = cards
(** Discard pile of cards played in discarded order. *)
type discard = card list
(** The hex currently occupied by the robber. The robber is always on a piece *)
type robber = piece

(** Keeps track of everything on the physical gameboard. One of four "big"
    components of the overall game state. *)
type board = map * structures * deck * discard * robber

(** [(id * cost1 * cost2)] is a (proposed) exchange of [cost1] from active
    for [cost2] with [id]. trades can only be initiated by the active player
    as part of their turn. *)
type trade = color * cost * cost

(** Current turn state keeps track of what has happened during the turn
    and is renewed when a new turn begins *)
type turn = {
  active : color;             (** The color of the active player *)
  dicerolled : roll option;   (** [Some roll] if dice were rolled this turn *)
  cardplayed : bool;
  cardsbought : cards;        (** Any cards a player buys during their turn are
                                  stored in turn.  cardsbought and are only
                                  transfered to the player's hand at the END of
                                  the turn *)
  tradesmade : int;           (** Number of trades made this turn *)
  pendingtrade : trade option (** A trade that's just been proposed, but not yet
                                  accepted/rejected.  *)
}

(** Five different move types that the game can request a player to make *)
type request = InitialRequest
             | RobberRequest
             | DiscardRequest
             | TradeRequest
             | ActionRequest

(** Color of the next player to be prompted * type of move requested
    this is updated after every move, for the following move.  *)
type next = color * request

(** ALL the information needed to represent the current game state **)
type state = board * player list * turn * next


(******************************************************************************)
(** {2 Moves}                                                                 *)
(******************************************************************************)

(** Resource type sold * resource type bought *)
type maritimetrade = resource * resource

(** Can only build on your turn *)
type build = BuildRoad of road
           | BuildTown of point
           | BuildCity of point
           | BuildCard

(** The hex the robber will move to, and color of an adjacent player to rob,
    if one is available *)
type robbermove = piece * color option

(** RoadBuilding, YearOfPlenty is also valid with only one road, resource *)
type playcard = PlayKnight of robbermove
              | PlayRoadBuilding of road * road option
              | PlayYearOfPlenty of resource * resource option
              | PlayMonopoly of resource


(** The types of actions a player can perform in response to an ActionRequest *)
type action = RollDice
            | MaritimeTrade of maritimetrade
            | DomesticTrade of trade
            | BuyBuild of build
            | PlayCard of playcard
            | EndTurn

(** The move types a player can make, in response to InitialRequest,
    RobberRequest, DiscardRequest, TradeRequest, and ActionRequest, respectively *)
type move = InitialMove of line (** [(p1 * p2)] as line places a town at [p1] and a road from [p1] to [p2] *)
          | RobberMove of robbermove
          | DiscardMove of cost (** Number of each resource the player wishes to discard, in B,W,O,G,L order *)
          | TradeResponse of bool (** true to accept the trade, false to reject. *)
          | Action of action


(******************************************************************************)
(** {2 Winning}                                                               *)
(******************************************************************************)

(** [Some c] if player [c] has won; None if game is still in progress *)
type 'a outcome = color option * 'a











(******************************************************************************)
(** {2 Map Constants}                                                         *)
(******************************************************************************)

let cMIN_PIECE_NUM = 0
let cMAX_PIECE_NUM = 18
let cNUM_PIECES = 19

let cMIN_POINT_NUM = 0
let cMAX_POINT_NUM = 53
let cNUM_POINTS = 54


(******************************************************************************)
(** {2 Game Constants}                                                        *)
(******************************************************************************)

(* Number of points a player needs to win *)
let cWIN_CONDITION = 10

(* Dice roll that activates the robber *)
let cROBBER_ROLL = 7

(* Max. number of trades a player can make per turn *)
let cNUM_TRADES_PER_TURN = 7

(* Max. hand size a player can have before needing to discard when the
 * robber is activated *)
let cMAX_HAND_SIZE = 7

(* Default maritime trade ratio for a port that trades any resource *)
let cMARITIME_DEFAULT_RATIO = 4

(* Num. resources generated per town *)
let cRESOURCES_GENERATED_TOWN = 1
(* Num. resources generated per city *)
let cRESOURCES_GENERATED_CITY = 2

(* Cost to build the four build types *)
let cCOST_ROAD : cost = (1,0,0,0,1)
let cCOST_TOWN : cost = (1,1,0,1,1)
let cCOST_CITY : cost = (0,0,3,2,0)
let cCOST_CARD : cost = (0,1,1,1,0)

(* Max. num. of structures a player can have on the board *)
let cMAX_TOWNS_PER_PLAYER = 5
let cMAX_CITIES_PER_PLAYER = 4
let cMAX_ROADS_PER_PLAYER = 15

(* Minimum num. of knights a player must play to be awarded largest army *)
let cMIN_LARGEST_ARMY = 3
(* Minimum length of road a player must have to be awarded longest road *)
let cMIN_LONGEST_ROAD = 5

(* Victory points scored *)
let cVP_CARD = 1 (* per victory card *)
let cVP_TOWN = 1 (* per town *)
let cVP_CITY = 2 (* per city *)
let cVP_LONGEST_ROAD = 2 (* for owning longest road trophy *)
let cVP_LARGEST_ARMY = 2 (* for owning largest army trophy *)

(* Number of cards in initial deck *)
let cNUM_KNIGHT = 14
let cNUM_VICTORYPOINT = 5
let cNUM_ROADBUILDING = 2
let cNUM_YEAROFPLENTY = 2
let cNUM_MONOPOLY = 2


(******************************************************************************)
(** {2 Play Mechanics}                                                        *)
(******************************************************************************)

let cNUM_PLAYERS = 4
let cSTEP_TIME = 0.7
let cMOVE_TIME = 3
let cMAX_TURNS = 400
let cTIMEOUT_MOVE = Action(EndTurn)


(******************************************************************************)
(** {2 New Game Defaults (Beginner Board)}                                    *)
(******************************************************************************)

let cDEFAULT_HEXES : hex list = 
          [(Forest, 11); (Pasture, 12); (Field, 9);
      (Hill, 4); (Mountain, 6); (Hill, 5); (Pasture, 10);
(Desert, 0); (Forest, 3); (Field, 11); (Forest, 4); (Field, 8);
      (Hill, 8); (Pasture, 10); (Pasture, 9); (Mountain, 3); 
           (Mountain, 5); (Field, 2); (Forest, 6)]
           
let cDEFAULT_PORTS : port list = 
  [((0, 1), 3, Any);
  ((3, 4), 2, PortResource(Wool));
  ((14, 15), 3, Any);
  ((7, 17), 2, PortResource(Ore));
  ((26, 37), 3, Any);
  ((28, 38), 2, PortResource(Grain));
  ((45, 46), 2, PortResource(Brick));
  ((47, 48), 3, Any);
  ((50, 51), 2, PortResource(Lumber))]
    
let cDEFAULT_INTERSECTIONS : intersection list =
  let rec none_list lst n =
    if n = 0 then lst else none_list (None::lst) (n-1) in
  none_list [] cNUM_POINTS

let cDEFAULT_ROADS : road list = []
let cDEFAULT_MAP : map = (cDEFAULT_HEXES, cDEFAULT_PORTS)
let cDEFAULT_STRUCTURES : structures = (cDEFAULT_INTERSECTIONS, cDEFAULT_ROADS)
let cDEFAULT_DECK : cards =
  let set = [(cNUM_KNIGHT, Knight);
    (cNUM_VICTORYPOINT, VictoryPoint);
    (cNUM_ROADBUILDING, RoadBuilding);
    (cNUM_YEAROFPLENTY, YearOfPlenty);
    (cNUM_MONOPOLY, Monopoly)] in
  (* add card n times to lst *)
  let rec add_card lst card n = if n = 0 then lst else add_card (card::lst) card (n-1) in
  let cards = List.fold_left (fun lst (n, c) -> add_card lst c n) [] set in
    Reveal(cards)

let cDEFAULT_DISCARD : discard = []
let cDEFAULT_ROBBER : robber = 7
let cDEFAULT_BOARD : board =
  (cDEFAULT_MAP, cDEFAULT_STRUCTURES, cDEFAULT_DECK, cDEFAULT_DISCARD, cDEFAULT_ROBBER)

let cDEFAULT_HAND : hand = ((0,0,0,0,0), Reveal([]))
let cDEFAULT_TROPHIES : trophies = (0, false, false)

let cDEFAULT_COLORS : color list = [Blue; Red; Orange; White]

let cDEFAULT_PLAYERS : player list = 
  List.map (fun c -> (c, cDEFAULT_HAND, cDEFAULT_TROPHIES)) cDEFAULT_COLORS











let _ = Random.self_init()

(******************************************************************************)
(** {2 Option utils}                                                          *)
(******************************************************************************)

(** true if None, else false *)
let is_none o = o = None

(** Unwraps Some(x) -> x, fails on None *)
let get_some = function
  Some x -> x | None -> failwith "tried to get_some of None"


(******************************************************************************)
(** {2 Random}                                                                *)
(******************************************************************************)

(** Simulate sum of rolling two dice *)
let random_roll () : roll = ((Random.int 6) + (Random.int 6) + 2)

(** Choose a random color *)
let random_color() : color = 
  match Random.int 4 with
    | 0 -> Blue
    | 1 -> Red
    | 2 -> Orange
    | 3 -> White
    | _ -> failwith "uh oh!"


(** weighted_random items weights  returns an element from items at random
 * based on the corresponding unnormalized probabilities in weights.
 * The lists must be the same length, and the sum of weights must be nonzero.
 *)
let weighted_random (items : 'a list) (weights : int list) : 'a =
  if List.length items <> List.length weights then
    failwith "weighted_random list length mismatch"
  else
    let sum = List.fold_left (+) 0 weights in
    if sum = 0 then failwith "weighted_random of all zero weights"
    else (* Find position by CDF *)
      let rand = Random.int sum in
      let choose (choice, i) x w = 
        match choice with
          | Some(c) -> (Some(c), i)
          | None ->
            let i = i + w in
            let c = if i > rand then Some(x) else None in
              (c, i)
      in
        get_some (fst (List.fold_left2 choose (None, 0) items weights))


(******************************************************************************)
(** {2 List utils}                                                            *)
(******************************************************************************)

(** Returns the number of elements in lst satisfying the predicate *)
let list_count (p : 'a -> bool) (lst : 'a list) : int =
  List.length (List.filter p lst)

(** Returns the sum of all elements in an int list *)
let list_sum : int list -> int = List.fold_left (+) 0
(** Returns the max of all elements in an int list *)
let list_max : int list -> int = List.fold_left max 0

(** Returns the index of the first element satisfying the predicate. Not [[]]-safe *)
let list_indexof (p : 'a -> bool) (lst : 'a list) : int =
  let rec index l n =
    match l with
      | [] -> failwith "indexof not found"
      | h::t -> if p h then n else index t (n+1)
  in
    index lst 0

(** Returns list with the first item satisfying the predicate removed. Not [[]]-safe *)
let rec list_memremove (p : 'a -> bool) (lst : 'a list) : 'a list =
  match lst with
    | [] -> failwith "no element to memremove"
    | h::t -> if p h then t else h::(list_memremove p t)

(** Returns Some(x) where x is an element in the list, or None for the empty list *)
let pick_random (lst : 'a list) : 'a option = 
  if lst = [] then None
  else Some (List.nth lst (Random.int (List.length lst)))

(** Returns [(x, lst - {x})], where x is a list element. Not [[]]-safe *)
let pick_one (lst : 'a list) : ('a * 'a list) =
  match lst with
  | [] -> failwith "cannot pick_one of []"
  | _ ->
    let n = Random.int (List.length lst) in
    let one = List.nth lst n in
    let split (l,num) x = 
      let l = (if num = n then l else (x::l)) in (l, num+1) 
    in
    let rem = fst (List.fold_left split ([],0) lst) in
      (one, rem)

(** Returns the list with elements in randomized order *)
let randomize (lst : 'a list) : 'a list =
  let rec iterate input output =
    match input with
      | [] -> output
      | _ -> let (next, remaining) = pick_one input in
        iterate remaining (next::output)
  in
    iterate lst []

(******************************************************************************)
(** {2 Hidden Utils}                                                          *)
(******************************************************************************)

(** Hides the cards if they are revealed *)
let hide : cards -> cards = function
  | Hidden(h) -> Hidden(h)
  | Reveal(cs) -> Hidden(List.length cs)

(** Unwrap Reveal(c) -> c *)
let reveal : cards -> card list =
  function
  | Reveal(c) -> c
  | Hidden(n) -> failwith "attempted to reveal hidden"

(** Wrap c -> Reveal(c) *)
let wrap_reveal : card list -> cards = 
  fun c -> Reveal(c)

(** Adds a card to a Reveal *)
let append_card (cs : cards) (c : card) : cards =
  wrap_reveal ((reveal cs)@[c])


(******************************************************************************)
(** {2 Cost Utils}                                                            *)
(******************************************************************************)

(** Sum of all the elements in the cost tuple *)
let sum_cost ((b,w,o,l,g) : cost) : int = b+w+o+l+g 

(** Maps a function across the cost tuple *)
let map_cost (f : int -> 'a) ((b,w,o,l,g) : cost) = (f b,f w,f o,f l,f g)

(** Maps a function across two cost tuples *)
let map_cost2 (f : int -> int -> 'a) (c1 : cost) (c2 : cost) =
  let (b1,w1,o1,l1,g1) = c1 in
  let (b2,w2,o2,l2,g2) = c2 in
    (f b1 b2, f w1 w2, f o1 o2, f l1 l2, f g1 g2)


(******************************************************************************)
(** {2 Match utils}                                                           *)
(******************************************************************************)

(** Returns Some(r) where r is the resource produced by terrain, or None if none is produced *)
let resource_of_terrain (terrain : terrain) : resource option = 
  match terrain with
    | Hill -> Some Brick 
    | Pasture -> Some Wool
    | Mountain -> Some Ore
    | Field -> Some Grain
    | Forest -> Some Lumber
    | Desert -> None

(** Returns a cost where there is one of the resource specified, and zero of all others *)
let single_resource_cost (resource : resource) : cost = 
  match resource with
    | Brick ->  (1,0,0,0,0)
    | Wool ->   (0,1,0,0,0)
    | Ore ->    (0,0,1,0,0)
    | Grain ->  (0,0,0,1,0)
    | Lumber -> (0,0,0,0,1)

(** Returns the number of resources generated by a type of settlement *)
let settlement_num_resources (set : settlement) : int =
  match set with
    | Town -> cRESOURCES_GENERATED_TOWN
    | City -> cRESOURCES_GENERATED_CITY

(** Returns the number of a specific resource in an inventory *)
let num_resource_in_inventory (inv : inventory) (res : resource) : int =
  let (b,w,o,l,g) = inv in
    match res with
      | Brick -> b
      | Wool -> w
      | Ore -> o
      | Grain -> g
      | Lumber -> l

(** Returns the cost of building a build *)
let cost_of_build (build: build) : cost = 
  match build with
    | BuildRoad(_) -> cCOST_ROAD
    | BuildTown(_) -> cCOST_TOWN
    | BuildCity(_) -> cCOST_CITY
    | BuildCard -> cCOST_CARD

(** Returns the next turn's color in normal forward order *)
let next_turn (color : color) : color =
  match color with
    | Blue -> Red
    | Red -> Orange
    | Orange -> White
    | White -> Blue

(** Returns the next turn's color in reverse order *)
let prev_turn (color : color) : color =
  match color with
    | Blue -> White
    | Red -> Blue
    | Orange -> Red
    | White -> Orange

(** Identifies the card variant based on the playcard *)
let card_of_playcard (play : playcard) : card =
  match play with
    | PlayKnight(_) -> Knight
    | PlayRoadBuilding(_) -> RoadBuilding
    | PlayYearOfPlenty(_) -> YearOfPlenty
    | PlayMonopoly(_) -> Monopoly

(** Returns a list of points reachable from point in one road length *)
let adjacent_points (point : point) : point list = 
  match point with
    | 0 -> [1;8] | 1 -> [0;2] | 2 -> [1;3;10] | 3 -> [2;4] | 4 -> [3;5;12] | 5 -> [4;6] | 6 -> [5;14]
    | 7 -> [8;17] | 8 -> [0;7;9] | 9 -> [8;10;19] | 10 -> [2;9;11] | 11 -> [10;12;21] | 12 -> [4;11;13] | 13 -> [12;14;23]  | 14 -> [6;13;15] | 15 -> [14;25]
    | 16 -> [17;27] | 17 -> [7;16;18] | 18 -> [17;19;29] | 19 -> [9;18;20] | 20 -> [19;21;31] | 21 -> [11;20;22] | 22 -> [21;23;33] | 23 -> [13;22;24] | 24 -> [23;25;35] | 25 -> [15;24;26]
    | 26 -> [25;37] | 27 -> [16;28] | 28 -> [27;29;38] | 29 -> [18;28;30] | 30 -> [29;31;40] | 31 -> [20;30;32] | 32 -> [31;33;42] | 33 -> [22;32;34] | 34 -> [33;35;44] | 35 -> [24;34;36] | 36 -> [35;37;46] | 37 -> [26;36]
    | 38 -> [28;39] | 39 -> [38;40;47] | 40 -> [30;39;41] | 41 -> [40;42;49] | 42 -> [32;41;43] | 43 -> [42;44;51] | 44 -> [34;43;45] | 45 -> [44;46;53] | 46 -> [36;45]
    | 47 -> [39;48] | 48 -> [47;49] | 49 -> [41;48;50] | 50 -> [49;51] | 51 -> [43;50;52] | 52 -> [51;53] | 53 -> [45;52]
    | _ -> failwith "invalid point"

(** Returss a list of hex indices corresponding to pieces that point borders *)
let adjacent_pieces (point : point) : piece list =
  match point with
    | 0 -> [0] | 1 -> [0] | 2 -> [0;1] | 3 -> [1] | 4 -> [1;2] | 5 -> [2] | 6 -> [2] 
    | 7 -> [3] | 8 -> [0;3] | 9 -> [0;3;4] | 10 -> [0;1;4] | 11 -> [1;4;5] | 12 -> [1;2;5] | 13 -> [2;5;6] | 14 -> [2;6] | 15 -> [6] 
    | 16 -> [7] | 17 -> [3;7] | 18 -> [3;7;8] | 19 -> [3;4;8] | 20 -> [4;8;9] | 21 -> [4;5;9] | 22 -> [5;9;10] | 23 -> [5;6;10] | 24 -> [6;10;11] | 25 -> [6;11] | 26 -> [11] 
    | 27 -> [7] | 28 -> [7;12] | 29 -> [7;8;12] | 30 -> [8;12;13] | 31 -> [8;9;13] | 32 -> [9;13;14] | 33 -> [9;10;14] | 34 -> [10;14;15] | 35 -> [10;11;15] | 36 -> [11;15] | 37 -> [11] 
    | 38 -> [12] | 39 -> [12;16] | 40 -> [12;13;16] | 41 -> [13;16;17] | 42 -> [13;14;17] | 43 -> [14;17;18] | 44 -> [14;15;18] | 45 -> [15;18] | 46 -> [15] 
    | 47 -> [16] | 48 -> [16] | 49 -> [16;17] | 50 -> [17] | 51 -> [17;18] | 52 -> [18] | 53 -> [18] 
    | _ -> failwith "invalid point"

(** Returns the indices of all points bordering the hex piece *)
let piece_corners (piece : piece) : point list =
  match piece with
                        | 0 -> [0;1;2;8;9;10] | 1 -> [2;3;4;10;11;12] | 2 -> [4;5;6;12;13;14]
            | 3 -> [7;8;9;17;18;19] | 4 -> [9;10;11;19;20;21] | 5 -> [11;12;13;21;22;23] | 6 -> [13;14;15;23;24;25]
| 7 -> [16;17;18;27;28;29] | 8 -> [18;19;20;29;30;31] | 9 -> [20;21;22;31;32;33] | 10 -> [22;23;24;33;34;35] | 11 -> [24;25;26;35;36;37]
            | 12 -> [28;29;30;38;39;40] | 13 -> [30;31;32;40;41;42] | 14 -> [32;33;34;42;43;44] | 15 -> [34;35;36;44;45;46]
                | 16 -> [39;40;41;47;48;49] | 17 -> [41;42;43;49;50;51] | 18 -> [43;44;45;51;52;53]
    | _ -> failwith "invalid piece number"



(******************************************************************************)
(** {2 Longest road}                                                          *)
(******************************************************************************)

(** Returns the road-length of a player's longest road *)
let longest_road (c : color) (roads : road list) (inters : intersection list) : int =
  let no_enemy (p : point) : bool =
    match List.nth inters p with
      | None -> true
      | Some(color, _) -> c = color
  in
  (* sets are split by enemy settlements if there are no other connections *)
  let includes (lst : line list) ((a,b) : line) : bool = 
    let includes_one (c,d) = 
      ((a = c || b = c) && no_enemy c) || ((a = d || b = d) && no_enemy d)
    in
      List.exists includes_one lst
  in
  (* partition the set of lines into connected components *)
  let split_sets (lines : line list) : line list list =
    (* returns the partition of completed set * remaining lines *)
    let rec one_set (start : line list) (rest : line list) : (line list * line list) =
      let (added,r) = List.partition (includes start) rest in
      match added with
        | [] -> start, rest
        | added -> one_set (added@start) r
    in
    (* completely partition a set of lines *)
    let rec build_sets (sets : line list list) (lines_remaining : line list) : line list list = 
      match lines_remaining with
        | [] -> sets
        | h::t -> (
          let (new_set, remaining) = one_set [h] t in
            build_sets (new_set::sets) remaining)
    in
      build_sets [] lines
  in
  (* traverse the set from each endpoint *)
  let count_length (set : line list) : int =
    (* return lst with all duplicates removed *)
    let rec remove_duplicates lst =
      match lst with
        | h::t -> let rest = remove_duplicates t in
          if List.mem h t then rest else h::rest
        | [] -> []
    in
    (* list of all the points in the set of lines *)
    let all_points set =
      remove_duplicates (List.flatten (List.map (fun (a,b) -> [a;b]) set))
    in
    (* return points in the set that are connected to p *)
    let connected_points p =
      let rec find lst points =
        match lst with
          | (a, b)::t ->
            if a = p then find t (b::points)
            else if b = p then find t (a::points)
            else find t points
          | [] -> points
      in
        find set []
    in
    let not_including lst i = List.filter ((<>) i) lst in
    (* find the maximum length traversing from start *)
    let traverse points start =
      (* count the points traversed *)
      let rec traverse_count ps s =
        match not_including ps s with
          | [] -> 1
          | rest ->
            let connected = connected_points s in
            let paths = List.filter (fun x -> List.mem x rest) connected in
            let lengths = List.map (traverse_count rest) paths in
              (list_max lengths) + 1
      in
        (* length is 1 less than points traversed *)
        (traverse_count points start) - 1
    in
    (* Find the max length starting from each point *)
    let setpoints = all_points set in
    let lengths = List.map (traverse setpoints) setpoints in
        list_max lengths
  in
  (* Maximum of all of the player's sets *)
  let my_lines = List.map snd roads in
  let sets = split_sets my_lines in
    list_max (List.map count_length sets)

   
(******************************************************************************)
(** {2 State generation}                                                      *)
(******************************************************************************)

(** Returns a blank turn with active = color *)
let new_turn (c : color) : turn = {active = c;
                                   dicerolled = None;
                                   cardplayed = false;
                                   cardsbought = Reveal([]);
                                   tradesmade = 0;
                                   pendingtrade = None}


(** Default initial state generator *)
let gen_initial_state () : state = 
  let c = random_color() in
    (cDEFAULT_BOARD, cDEFAULT_PLAYERS, new_turn c, (c, InitialRequest))


(** Produces random initial states for experimenting with bots *)
let gen_random_initial_state () : state = 
  let gen_random_map () : map =
    let (terrains, rolls) = List.split cDEFAULT_HEXES in
    let hexes = List.combine (randomize terrains) (randomize rolls) in
      (hexes, cDEFAULT_PORTS)
  in
  let gen_random_board () : board =
    let (hexes, _) as map = gen_random_map() in
    let robber = list_indexof (fun (t,_) -> t = Desert) hexes in
      (map, cDEFAULT_STRUCTURES, cDEFAULT_DECK, cDEFAULT_DISCARD, robber)
  in
  let c = random_color() in
    (gen_random_board(), cDEFAULT_PLAYERS, new_turn c, (c, InitialRequest))



type gPlayer = {
  gPColor                    : color;
  gPInventory                : inventory;
  gPCard                     : cards;
  gPTrophies                  : trophies;
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





