#lang typed/racket

;; ---------------------------------------------------------------------------------------------------
;; a data representation for internal game states 
;; -- inspecting the game state 
;; -- manipulating the game state 
;; including a data structure for internalizing the state of the players

(require
 require-typed-check/unsafe
 "board-adapted.rkt"
 "../base/types.rkt"
 typed/racket/class)

(unsafe-require/typed/check "state.rkt"
  (#:struct player (
    [name : String]
    [tiles : (Listof Tile)]
    [money : Cash]
    [shares : Shares]
    [external : (Option (Instance Player%))]))
  (#:struct state
   ([board : Board]
    [players : (Listof Player)]
    [tiles : (Listof Tile)]
    [hotels : (Listof Hotel)]
    [shares : Shares]
    [bad : (Listof Player)]))
)

(define-type State state)
(define-type Player player)

;; -----------------------------------------------------------------------------

(define-type Decisions (Listof (List Player (Listof (List Hotel Boolean)))))
(define-type Score (Listof (List String Cash)))

(define-type Administrator%
  (Class
   (init-field
    (next-tile (-> (Listof Tile) Tile)))
   (sign-up (-> String (Instance Player%) String))
   (show-players (-> (Listof String)))
   (run (->* (Natural) (#:show (-> Void)) RunResult))
   ))

(define-type Turn%
  (Class
   (init-field (current-state State))
   (field
    (board Board)
    (current Player)
    (cash Cash)
    (tiles (Listof Tile))
    (shares Shares)
    (hotels (Listof Hotel))
    (players (Listof Player)))
   (reconcile-shares (-> Shares Shares))
   (eliminated (-> (Listof Player)))
   (place-called (-> Boolean))
   (decisions (-> (Values (Option Tile) (Option Hotel) Decisions)))
   ;; Precondition: (send this place-called)
   (place (-> Tile Hotel (U Void (Listof Player))))
   ))

(define-type Player%
  (Class
   (init-field
    [name String]
    [choice Strategy])
   (field
    [*players (Listof Player)]
    [*bad (Listof Player)])
   (go (-> (Instance Administrator%) Void))
   (setup (-> State Void))
   (take-turn (-> (Instance Turn%) (Values (Option Tile) (Option Hotel) (Listof Hotel))))
   (keep (-> (Listof Hotel) (Listof Boolean)))
   (receive-tile (-> Tile Void))
   (inform (-> State Void))
   (the-end (-> State Any Void))))

(define-type RunResult (List (U 'done 'exhausted 'score 'IMPOSSIBLE) Any (Listof State)))
(define-type Strategy (-> (Instance Turn%) (Values (Option Tile) (Option Hotel) (Listof Hotel))))

;; -----------------------------------------------------------------------------

(provide
 (struct-out player)
 (struct-out state)
 Score
 Player
 State
 Decisions
 ;; --
 Administrator%
 Turn%
 Player%
 RunResult
 Strategy
)



(module* admin typed/racket
  (require (submod "..")
   require-typed-check
   "board-adapted.rkt"
   "../base/types.rkt"
   typed/racket/class)
  (require/typed/check "state.rkt"
    (player-name (-> player String))
    [player-tiles (-> player (Listof Tile))]
    [player-money (-> player Cash)]
    [player-shares (-> player Shares)]
    [player-external (-> player (Option (Instance Player%)))]
    [state-board (-> state Board)]
    [state-players (-> state (Listof Player))]
    [state-tiles (-> state (Listof Tile))]
    [state-hotels (-> state (Listof Hotel))]
    [state-shares (-> state Shares)]
    [state-bad (-> state (Listof Player))]
    (score? (-> Any Boolean))
    (*create-player (-> String Cash Shares (Listof Tile) Player))
    (player0 (-> String Tile Tile Tile Tile Tile Tile (Instance Player%) Player))
    (state0 (-> Player * State))
    (state-sub-shares (-> State Shares State))
    (*cs0 (-> String * State))
    (*create-state (-> Board (Listof Player) State))
    (state-place-tile (->* (State Tile) ((Option Hotel)) State))
    (state-move-tile (-> State Tile State))
    (state-next-turn (-> State State))
    (state-remove-current-player (-> State State))
    (state-eliminate (-> State (Listof Player) State))
    (state-current-player (-> State Player))
    (state-buy-shares (-> State (Listof Hotel) State))
    (state-return-shares (->* [State Decisions] [Board] State))
    (state-score (-> State (Listof (List String Cash))))
    (state-final? (-> State Boolean)))
  (provide (all-from-out (submod ".."))
    player-name player-tiles player-money player-shares player-external state-board state-players state-tiles state-hotels state-shares state-bad
  state-current-player state-sub-shares player0 *create-player state0 state-place-tile state-buy-shares state-return-shares state-move-tile state-next-turn state-remove-current-player state-eliminate state-score state-final?  *create-state *cs0))

(module* main typed/racket
  (require (submod "..")
   require-typed-check
   "board-adapted.rkt"
   "../base/types.rkt"
   typed/racket/class)
  (require/typed/check "state.rkt"
    (player-name (-> player String))
    [player-tiles (-> player (Listof Tile))]
    [player-money (-> player Cash)]
    [player-shares (-> player Shares)]
    [player-external (-> player (Option (Instance Player%)))]
    [state-board (-> state Board)]
    [state-players (-> state (Listof Player))]
    [state-tiles (-> state (Listof Tile))]
    [state-hotels (-> state (Listof Hotel))]
    [state-shares (-> state Shares)]
    [state-bad (-> state (Listof Player))]
    (score? (-> Any Boolean))
    (*create-player (-> String Cash Shares (Listof Tile) Player))
    (player0 (-> String Tile Tile Tile Tile Tile Tile (Instance Player%) Player))
    (state0 (-> Player * State))
    (state-sub-shares (-> State Shares State))
    (*cs0 (-> String * State))
    (*create-state (-> Board (Listof Player) State))
    (state-place-tile (->* (State Tile) ((Option Hotel)) State))
    (state-move-tile (-> State Tile State))
    (state-next-turn (-> State State))
    (state-remove-current-player (-> State State))
    (state-eliminate (-> State (Listof Player) State))
    (state-current-player (-> State Player))
    (state-buy-shares (-> State (Listof Hotel) State))
    (state-return-shares (->* [State Decisions] [Board] State))
    (state-score (-> State (Listof (List String Cash))))
    (state-final? (-> State Boolean)))
  (provide (all-from-out (submod ".."))
    player-name player-tiles player-money player-shares player-external state-board state-players state-tiles state-hotels state-shares state-bad
  state-current-player state-sub-shares player0 *create-player state0 state-place-tile state-buy-shares state-return-shares state-move-tile state-next-turn state-remove-current-player state-eliminate state-score state-final?  *create-state *cs0))

(module* player typed/racket
  (require (submod "..")
   require-typed-check
   "board-adapted.rkt"
   "../base/types.rkt"
   typed/racket/class)
  (require/typed/check "state.rkt"
    (player-name (-> player String))
    [player-tiles (-> player (Listof Tile))]
    [player-money (-> player Cash)]
    [player-shares (-> player Shares)]
    [player-external (-> player (Option (Instance Player%)))]
    [state-board (-> state Board)]
    [state-players (-> state (Listof Player))]
    [state-tiles (-> state (Listof Tile))]
    [state-hotels (-> state (Listof Hotel))]
    [state-shares (-> state Shares)]
    [state-bad (-> state (Listof Player))]
    (score? (-> Any Boolean))
    (*create-player (-> String Cash Shares (Listof Tile) Player))
    (player0 (-> String Tile Tile Tile Tile Tile Tile (Instance Player%) Player))
    (state0 (-> Player * State))
    (state-sub-shares (-> State Shares State))
    (*cs0 (-> String * State))
    (*create-state (-> Board (Listof Player) State))
    (state-place-tile (->* (State Tile) ((Option Hotel)) State))
    (state-move-tile (-> State Tile State))
    (state-next-turn (-> State State))
    (state-remove-current-player (-> State State))
    (state-eliminate (-> State (Listof Player) State))
    (state-current-player (-> State Player))
    (state-buy-shares (-> State (Listof Hotel) State))
    (state-return-shares (->* [State Decisions] [Board] State))
    (state-score (-> State (Listof (List String Cash))))
    (state-final? (-> State Boolean)))
  (provide (all-from-out (submod ".."))
    player-name player-tiles player-money player-shares player-external state-board state-players state-tiles state-hotels state-shares state-bad
  state-current-player state-sub-shares player0 *create-player state0 state-place-tile state-buy-shares state-return-shares state-move-tile state-next-turn state-remove-current-player state-eliminate state-score state-final?  *create-state *cs0))

(module* strategy typed/racket
  (require (submod "..")
   require-typed-check
   "board-adapted.rkt"
   "../base/types.rkt"
   typed/racket/class)
  (require/typed/check "state.rkt"
    (player-name (-> player String))
    [player-tiles (-> player (Listof Tile))]
    [player-money (-> player Cash)]
    [player-shares (-> player Shares)]
    [player-external (-> player (Option (Instance Player%)))]
    [state-board (-> state Board)]
    [state-players (-> state (Listof Player))]
    [state-tiles (-> state (Listof Tile))]
    [state-hotels (-> state (Listof Hotel))]
    [state-shares (-> state Shares)]
    [state-bad (-> state (Listof Player))]
    (score? (-> Any Boolean))
    (*create-player (-> String Cash Shares (Listof Tile) Player))
    (player0 (-> String Tile Tile Tile Tile Tile Tile (Instance Player%) Player))
    (state0 (-> Player * State))
    (state-sub-shares (-> State Shares State))
    (*cs0 (-> String * State))
    (*create-state (-> Board (Listof Player) State))
    (state-place-tile (->* (State Tile) ((Option Hotel)) State))
    (state-move-tile (-> State Tile State))
    (state-next-turn (-> State State))
    (state-remove-current-player (-> State State))
    (state-eliminate (-> State (Listof Player) State))
    (state-current-player (-> State Player))
    (state-buy-shares (-> State (Listof Hotel) State))
    (state-return-shares (->* [State Decisions] [Board] State))
    (state-score (-> State (Listof (List String Cash))))
    (state-final? (-> State Boolean)))
  (provide (all-from-out (submod ".."))
    player-name player-tiles player-money player-shares player-external state-board state-players state-tiles state-hotels state-shares state-bad
  state-current-player state-sub-shares player0 *create-player state0 state-place-tile state-buy-shares state-return-shares state-move-tile state-next-turn state-remove-current-player state-eliminate state-score state-final?  *create-state *cs0))

(module* tree typed/racket
  (require (submod "..")
   require-typed-check
   "board-adapted.rkt"
   "../base/types.rkt"
   typed/racket/class)
  (require/typed/check "state.rkt"
    (player-name (-> player String))
    [player-tiles (-> player (Listof Tile))]
    [player-money (-> player Cash)]
    [player-shares (-> player Shares)]
    [player-external (-> player (Option (Instance Player%)))]
    [state-board (-> state Board)]
    [state-players (-> state (Listof Player))]
    [state-tiles (-> state (Listof Tile))]
    [state-hotels (-> state (Listof Hotel))]
    [state-shares (-> state Shares)]
    [state-bad (-> state (Listof Player))]
    (score? (-> Any Boolean))
    (*create-player (-> String Cash Shares (Listof Tile) Player))
    (player0 (-> String Tile Tile Tile Tile Tile Tile (Instance Player%) Player))
    (state0 (-> Player * State))
    (state-sub-shares (-> State Shares State))
    (*cs0 (-> String * State))
    (*create-state (-> Board (Listof Player) State))
    (state-place-tile (->* (State Tile) ((Option Hotel)) State))
    (state-move-tile (-> State Tile State))
    (state-next-turn (-> State State))
    (state-remove-current-player (-> State State))
    (state-eliminate (-> State (Listof Player) State))
    (state-current-player (-> State Player))
    (state-buy-shares (-> State (Listof Hotel) State))
    (state-return-shares (->* [State Decisions] [Board] State))
    (state-score (-> State (Listof (List String Cash))))
    (state-final? (-> State Boolean)))
  (provide (all-from-out (submod ".."))
    player-name player-tiles player-money player-shares player-external state-board state-players state-tiles state-hotels state-shares state-bad
  state-current-player state-sub-shares player0 *create-player state0 state-place-tile state-buy-shares state-return-shares state-move-tile state-next-turn state-remove-current-player state-eliminate state-score state-final?  *create-state *cs0))

