open Definition
open Constant
open Util
open Print
include GameType


(**********************************************************************)
(******                {HexList Model functions}                 ******)
(**********************************************************************)

(* function used to fold_left on a list *)
let leftFoldHexList = List.fold_left

(* return the nth element of lst *)
let nthOfHexList = List.nth

(* return the index of first element satisfy the predicate *)
let indexOfHexList = list_indexof 

let sizeOfHexList = List.length

let mapList = List.map

let mapHexList = List.map

let memHexList = List.mem

let filterOnHexList = List.filter

let forAllHexList = List.for_all

let existsHexList = List.exists

let mapiHexList = List.mapi

let addToHexList ele lst = [ele]@lst 

let appendHexLists = List.append

let checkHexListNull (lst:'a list) : bool = 
  match lst with
  | [] -> true
  | _  -> false

(* function used to set nth of list to a new element, if
n is greated than the list size, than return original list *)
let setNthHexList (n:int) (ele:'a) (lst:'a list) : 'a list = 
  List.mapi (fun index e -> if (index = n) then ele else e) lst


(**********************************************************************)
(******                {portList Model functions}                 ******)
(**********************************************************************)

(* function used to fold_left on a list *)
let leftFoldPortList = List.fold_left

(* return the nth element of lst *)
let nthOfPortList = List.nth

(* return the index of first element satisfy the predicate *)
let indexOfPortList = list_indexof 

let sizeOfPortList = List.length

let mapPortList = List.map

let memPortList = List.mem

let filterOnPortList = List.filter

let forAllPortList = List.for_all

let existsPortList = List.exists

let mapiPortList = List.mapi

let addToPortList ele lst = [ele]@lst 

let appendPortLists = List.append

let checkPortListNull (lst:'a list) : bool = 
  match lst with
  | [] -> true
  | _  -> false

(* function used to set nth of list to a new element, if
n is greated than the list size, than return original list *)
let setNthPortList (n:int) (ele:'a) (lst:'a list) : 'a list = 
  List.mapi (fun index e -> if (index = n) then ele else e) lst



(**********************************************************************)
(******                {InterList Model functions}               ******)
(**********************************************************************)

(* function used to fold_left on a list *)
let leftFoldInterList = List.fold_left

(* return the nth element of lst *)
let nthOfInterList = List.nth

(* return the index of first element satisfy the predicate *)
let indexOfInterList = list_indexof 

let sizeOfInterList = List.length

let mapInterList = List.map

let memInterList = List.mem

let filterOnInterList = List.filter

let forAllInterList = List.for_all

let existsInterList = List.exists

let mapiInterList = List.mapi

let addToInterList ele lst = [ele]@lst 

let appendInterLists = List.append

let checkInterListNull (lst:'a list) : bool = 
  match lst with
  | [] -> true
  | _  -> false

(* function used to set nth of list to a new element, if
n is greated than the list size, than return original list *)
let setNthInterList (n:int) (ele:'a) (lst:'a list) : 'a list = 
  List.mapi (fun index e -> if (index = n) then ele else e) lst


(**********************************************************************)
(******                {RoadList Model functions}                ******)
(**********************************************************************)

(* function used to fold_left on a list *)
let leftFoldRoadList = List.fold_left

(* return the nth element of lst *)
let nthOfRoadList = List.nth

(* return the index of first element satisfy the predicate *)
let indexOfRoadList = list_indexof 

let sizeOfRoadList = List.length

let mapRoadList = List.map

let memRoadList = List.mem

let filterOnRoadList = List.filter

let forAllRoadList = List.for_all

let existsRoadList = List.exists

let mapiRoadList = List.mapi

let addToRoadList ele lst = [ele]@lst 

let appendRoadLists = List.append

let findRoadList = List.find

let checkRoadListNull (lst:'a list) : bool = 
  match lst with
  | [] -> true
  | _  -> false

(* function used to set nth of list to a new element, if
n is greated than the list size, than return original list *)
let setNthRoadList (n:int) (ele:'a) (lst:'a list) : 'a list = 
  List.mapi (fun index e -> if (index = n) then ele else e) lst


(**********************************************************************)
(******                {LineList Model functions}                ******)
(**********************************************************************)

(* function used to fold_left on a list *)
let leftFoldLineList = List.fold_left

(* return the nth element of lst *)
let nthOfLineList = List.nth

(* return the index of first element satisfy the predicate *)
let indexOfLineList = list_indexof 

let sizeOfLineList = List.length

let mapLineList = List.map

let memLineList = List.mem

let filterOnLineList = List.filter

let forAllLineList = List.for_all

let existsLineList = List.exists

let mapiLineList = List.mapi

let addToLineList ele lst = [ele]@lst 

let appendLineLists = List.append

let checkLineListNull (lst:'a list) : bool = 
  match lst with
  | [] -> true
  | _  -> false

(* function used to set nth of list to a new element, if
n is greated than the list size, than return original list *)
let setNthLineList (n:int) (ele:'a) (lst:'a list) : 'a list = 
  List.mapi (fun index e -> if (index = n) then ele else e) lst

(**********************************************************************)
(******                {PointList Model functions}                ******)
(**********************************************************************)

(* function used to fold_left on a list *)
let leftFoldPointList = List.fold_left

(* return the nth element of lst *)
let nthOfPointList = List.nth

(* return the index of first element satisfy the predicate *)
let indexOfPointList = list_indexof 

let sizeOfPointList = List.length

let mapPointList = List.map

let memPointList = List.mem

let filterOnPointList = List.filter

let forAllPointList = List.for_all

let existsPointList = List.exists

let mapiPointList = List.mapi

let addToPointList ele lst = [ele]@lst 

let appendPointLists = List.append

let findPointList = List.find

let checkPointListNull (lst:'a list) : bool = 
  match lst with
  | [] -> true
  | _  -> false

(* function used to set nth of list to a new element, if
n is greated than the list size, than return original list *)
let setNthPointList (n:int) (ele:'a) (lst:'a list) : 'a list = 
  List.mapi (fun index e -> if (index = n) then ele else e) lst



(**********************************************************************)
(******                 {Deck Model functions}                   ******)
(**********************************************************************)

(* function used to fold_left on a list *)
let leftFoldDeckList = List.fold_left

(* return the nth element of lst *)
let nthOfDeckList = List.nth

(* return the index of first element satisfy the predicate *)
let indexOfDeckList = list_indexof 

let sizeOfDeckList = List.length

let mapDeckList = List.map

let memDeckList = List.mem

let filterOnDeckList = List.filter

let forAllDeckList = List.for_all

let existsDeckList = List.exists

let mapiDeckList = List.mapi

let addToDeckList ele lst = [ele]@lst 

let appendDeckLists = List.append

let checkDeckListNull (lst:'a list) : bool = 
  match lst with
  | [] -> true
  | _  -> false

(* function used to set nth of list to a new element, if
n is greated than the list size, than return original list *)
let setNthDeckList (n:int) (ele:'a) (lst:'a list) : 'a list = 
  List.mapi (fun index e -> if (index = n) then ele else e) lst


(**********************************************************************)
(******             {CardList Model functions}                   ******)
(**********************************************************************)

(* function used to fold_left on a list *)
let leftFoldCardList = List.fold_left

(* return the nth element of lst *)
let nthOfCardList = List.nth

(* return the index of first element satisfy the predicate *)
let indexOfCardList = list_indexof 

let sizeOfCardList = List.length

let mapCardList = List.map

let memCardList = List.mem

let filterOnCardList = List.filter

let forAllCardList = List.for_all

let existsCardList = List.exists

let mapiCardList = List.mapi


let addToCardList ele lst = [ele]@lst 

let appendCardLists = List.append

let checkCardListNull (lst:'a list) : bool = 
  match lst with
  | [] -> true
  | _  -> false

(* function used to set nth of list to a new element, if
n is greated than the list size, than return original list *)
let setNthCardList (n:int) (ele:'a) (lst:'a list) : 'a list = 
  List.mapi (fun index e -> if (index = n) then ele else e) lst

(** Returns card list with the first item satisfying the 
  predicate removed. *)
let rec memRemoveCardList (p : 'a -> bool) (lst : 'a list) : 'a list =
  match lst with
    | [] -> []
    | h::t -> if p h then t else h::(list_memremove p t)

(**********************************************************************)
(******              {Cards related helper functions}             ******)
(**********************************************************************)

let addToCard (newCard:card) (allCards:cards) : cards = 
  match allCards with
  | Hidden _ -> allCards
  | Reveal cList -> Reveal (addToCardList newCard cList)

(* remove a specific card from cards and return the removed cards,
  if the card want to remove doesn't exist, return the original cards *)
let removeFromCards (card:card) (allCards:cards) :cards = 
  match allCards with
  | Hidden _ -> allCards
  | Reveal cList -> Reveal (memRemoveCardList (fun c -> c = card) cList)

(* combine two cards into one *)
let addCardsToCards (bought:cards) (hand:cards) : cards = 
  match bought with
  | Hidden _ -> hand
  | Reveal bList ->
    begin
      match hand with
      | Hidden _ -> hand
      | Reveal hList -> Reveal (appendCardLists bList hList)
    end

(* return whether the cards have a specific card in it *)
let memCards (card:card) (allCards:cards) : bool = 
  match allCards with
  | Hidden _ -> false
  | Reveal cList -> memCardList card cList


(**********************************************************************)
(******            {Player related helper functions}             ******)
(**********************************************************************)

(* match the intersection with specific color *)
let interMatchPlayer (inter:intersection) (player:color) : bool = 
  match inter with
  | None -> false
  | Some(c, _) -> c = player

(**********************************************************************)
(******     {Resource and Inventory related helper functions}    ******)
(**********************************************************************)

(* sum up two cost accordingly *)
let addCosts (cost1:cost) (cost2:cost) : cost = 
  map_cost2 (+) cost1 cost2 

(* cost1 - cost2 *)
let minusCosts (cost1:cost) (cost2:cost) : cost = 
  map_cost2 (-) cost1 cost2

(* multiply res according to different type of settlement *)
let multiRes (mul:int) (res:cost) : cost = map_cost ( ( * ) mul ) res

(* update the inventory with specific resource and delta,
delta can be negtive as well, need to check before call *)
let increaseResInInventory 
    (inv:inventory) (res:resource) (delta:int) : inventory = 
  let (b,w,o,g,l) = inv in
  match res with
  | Brick  ->    (b + delta, w, o, g, l)
  | Wool   ->    (b, w + delta, o, g, l)
  | Ore    ->    (b, w, o + delta, g, l)
  | Grain  ->    (b, w, o, g + delta, l)
  | Lumber ->    (b, w, o, g, l + delta)

(* set specific resource in the inventory with certain num,
  return new inventory *)
let setResInventory
    (inv:inventory) (res:resource) (num:int) : inventory = 
  let (b,w,o,g,l) = inv in
  match res with
  | Brick  ->    (num, w, o, g, l)
  | Wool   ->    (b, num, o, g, l)
  | Ore    ->    (b, w, num, g, l)
  | Grain  ->    (b, w, o, num, l)
  | Lumber ->    (b, w, o, g, num)


(* set a specific resource in inventory to zero, return original amount 
of that resource and new inventory *)
let setResToZero (inv:inventory) (res:resource) : (int * inventory) = 
  let (b,w,o,g,l) = inv in
  (match res with
  | Brick  ->    (b, (0, w, o, g, l))
  | Wool   ->    (w, (b, 0, o, g, l))
  | Ore    ->    (o, (b, w, 0, g, l))
  | Grain  ->    (g, (b, w, o, 0, l))
  | Lumber ->    (l, (b, w, o, g, 0)) )


(* compare cost1 with cost2, return true only if every subset in
  cost1 is greater than or equal to every subset accordingly in cost2 *)
let greaterThanEqual (cost1:cost) (cost2:cost) : bool = 
  let (bi,wi,oi,li,gi) = cost1 and (b,w,o,l,g) = cost2 in 
  bi >= b && wi >= w && oi >=o && li >= l && gi >= g 

(* compare cost1 with cost2, return true only if every subset in
  cost1 is greater than every subset accordingly in cost2 *)
let lessThanEqual (cost1:cost) (cost2:cost) : bool = 
  let (bi,wi,oi,li,gi) = cost1 and (b,w,o,l,g) = cost2 in 
  bi <= b && wi <= w && oi <=o && li <= l && gi <= g 


(**********************************************************************)
(******                {Player Model functions}                  ******)
(**********************************************************************)

(* function used to fold_left on a list *)
let leftFoldPlayerList = List.fold_left

(* return the nth element of lst *)
let nthOfPlayerList = List.nth

(* return the index of first element satisfy the predicate *)
let indexOfPlayerList = list_indexof 

let sizeOfPlayerList = List.length

let mapPlayerList = List.map

let memPlayerList = List.mem

let filterOnPlayerList = List.filter

let forAllPlayerList = List.for_all

let existsPlayerList = List.exists

let mapiPlayerList = List.mapi

let addToPlayerList ele lst = [ele]@lst 

let appendPlayerLists = List.append

let checkPlayerListNull (lst:'a list) : bool = 
  match lst with
  | [] -> true
  | _  -> false

(* function used to set nth of list to a new element, if
n is greated than the list size, than return original list *)
let setNthPlayerList (n:int) (ele:'a) (lst:'a list) : 'a list = 
  List.mapi (fun index e -> if (index = n) then ele else e) lst

(* update game with a new player status *)
let updatePlayer (game:game) (player:gPlayer) : game = 
  let target = player.gPColor in
  let pList = game.gPlayerList in
  let newPList = List.map 
    (fun p -> if(p.gPColor = target) then player else p) pList in
  {game with gPlayerList = newPList}

(* find the player from game with specific color *)
let findPlayer (game:game) (color:color) : gPlayer = 
  let pList = game.gPlayerList in
  List.find (fun p -> p.gPColor = color) pList

(*find the index of the player from game with specific color*)
let findPlayerIndex (game:game) (color:color) : int = 
  let pList = game.gPlayerList in 
  indexOfPlayerList (fun p -> p.gPColor = color) pList 

(* set specific resource in player to certain num *)
let setPlayerInvRes (player:gPlayer) (res:resource) (num:int) : gPlayer = 
  let inv = player.gPInventory in
  {player with
    gPInventory = setResInventory inv res num;}

(* remove a card from player's hand, if not exist, 
  just return the original player *)
let removeCardFromPlayer (player:gPlayer) (card:card) : gPlayer = 
  let pCards = player.gPCard in
  if (not(memCards card pCards)) then player
  else {player with gPCard = removeFromCards card player.gPCard;}

(* return the color of a player *)
let getPlayerColor (p:gPlayer) : color = p.gPColor

let hidePlayerCards (p:gPlayer) : gPlayer = 
  {p with gPCard = hide p.gPCard;}


