#lang typed/racket/shallow

;; ---------------------------------------------------------------------------------------------------
;; a data representation for internal game states 
;; -- inspecting the game state 
;; -- manipulating the game state 
;; including a data structure for internalizing the state of the players

(provide
 score?

 (struct-out player)
 ; player?
 ; ;; (-> Any Boolean)
 ; player-money
 ; ;; (-> Player Cash)
 ; player-tiles
 ; ;; (-> Player (Listof Tile))
 ; player-shares
 ; ;; (-> Player Shares)
 ; player-external
 ; ;; (-> Player Any)
 ; player-name
 ; ;; (-> Player String)

 player0
 ;; (->* (String Tile Tile Tile Tile Tile Tile) (Any) Player)

 (rename-out [ext:*create-player *create-player])
 ;; (-> String Cash Shares (Listof Tile) Player)
 ;; Precondition: distinct tiles
 ;; Precondition: (<= STARTER-TILES# (length tiles))

 (struct-out state)
 ;; state?
 ;; ;; (-> Any Boolean)
 ;; state-hotels
 ;; ;; (-> Any Boolean)
 ;; state-shares
 ;; ;; (-> State Shares)
 ;; state-tiles
 ;; ;; (-> State (Listof Tile))
 ;; state-board
 ;; ;; (-> State Board)
 ;; state-players
 ;; ;; (-> State (Listof Player))

 state-sub-shares
 ;; (-> State Shares State)

 state-current-player
 ;; (-> State Player)

 (rename-out [ext:state0 state0])
 ;; (->* () () #:rest (Listof Player) State)
 ;; Precondition: (λ (l) (distinct (apply append (map player-tiles l)))))

 (rename-out [ext:state-place-tile state-place-tile])
 ;; (->* (State Tile) (Hotel) State)
 ;; Precondition: (member t (player-tiles (state-current-player s)))
 ;; Precondition: (not (eq? (what-kind-of-spot (state-board s) t) IMPOSSIBLE))
 ;; Precondition: (or (unsupplied-arg? h) (memq (what-kind-of-spot (state-board s) t) (list FOUNDING MERGING)))
 ;; Precondition:
        ;; (let ((b (state-board s))
        ;;       (hotels (state-hotels s)))
        ;;   (==> (and (eq? (what-kind-of-spot b t) FOUNDING) (cons? hotels)) 
        ;;        (and (not (unsupplied-arg? h)) (member h hotels))))
 ;; Precondition:
        ;; (==> (eq? (what-kind-of-spot (state-board s) t) MERGING) 
        ;;      (and (not (unsupplied-arg? h)) 
        ;;           (let-values ([(w _) (merging-which (state-board s) t)])
        ;;             (member h w))))
 ;; Informal-Postcondition: if spot is MERGING, the hotel should be "large"

 (rename-out [ext:state-buy-shares state-buy-shares])
 ;; (-> State Shares-Order State)
 ;; Precondition:
        ;; (affordable? (state-board s) shares (player-money (state-current-player s)))
 ;; Precondition:
        ;; (let ([banker-s-shares (state-shares s)])
        ;;   (shares-available? banker-s-shares shares))

 (rename-out [ext:state-return-shares state-return-shares])
 ;; (->* [State (Listof (List Player (Listof (List Hotel Boolean))))] [Board] State)
 ;; TODO if optional board is specified, use the hotel sizes from there
 ;; Precondition:
        ;; (= (length (state-players s)) (length d))
  
 (rename-out [ext:state-move-tile state-move-tile])
 ;; (-> State Tile State)
 ;; Precondition:
        ;; (member t (state-tiles s))

 state-next-turn
 ;; (-> State State)

 state-remove-current-player
 ;; (-> State State)

 state-eliminate
 ;; (-> State (Listof Player) State)

 state-score
 ;; (-> State Score)

 state-final?
 ;; (-> State Boolean)
 
 (rename-out [ext:*create-state *create-state])
 ;; (-> Board (Listof Player) State)
 ;; Precondition: (combinable? lp)
 ;; Precondition: (distinct (apply append (board-tiles b) (map player-tiles lp)))

 (rename-out [ext:*cs0 *cs0])
 ;; (->* () #:rest (Listof String) State)
 ;; Precondition: strings are unique
 ;; Precondition: strings have fewer than 20 characters
 )

(: score? (-> Any Boolean))
(define (score? x*)
  (and (list? x*)
       (for/fold : (U Boolean Cash)
                 ([prev : (U Boolean Cash) #t])
                 ([x : Any (in-list x*)])
         (and prev
              (list? x)
              (not (null? x)) (not (null? (cdr x))) (null? (cddr x))
              (string? (car x))
              (cash? (cdr x))
              (if (cash? prev) (>= (assert prev real?) (assert (cdr x) real?)) #t)
              (assert (cdr x) exact-nonnegative-integer?)))
       #t))
;; (define score/c (and/c (listof (list/c string? cash?)) (sorted >= #:key second)))
(define-type Score (Listof (List String Cash)))
;; also, sorted in order of cash, descending

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION: 

(require
 require-typed-check
 "../base/types.rkt"
 (submod "board-adapted.rkt" state)
 )
(require/typed/check "basics.rkt"
  (ALL-HOTELS (Listof Hotel))
  (CASH0 Cash)
  (FINAL# Natural)
  (SAFE# Natural)
  (banker-shares0 Shares)
  (bonus (-> M*ority Hotel Natural Cash))
  (cash? (-> Any Boolean))
  (player-shares0 Shares)
  (price-per-share (-> Hotel Natural (Option Cash)))
  (shares++ (-> Shares Hotel Shares))
  (shares-- (-> Shares Hotel Shares))
  (shares->string (-> Shares String))
  (shares-available (-> Shares Hotel Share))
  (shares-available? (-> Shares (Listof Hotel) Boolean))
  (shares-combinable? (-> (Listof Shares) Boolean))
  (shares-order? (-> Any Boolean))
  (shares-minus (-> Shares Shares Shares))
  (shares-plus (-> Shares Shares Shares))
)
(require/typed/check "auxiliaries.rkt"
  (aux:partition (All (A B) (-> (Listof A) (-> A Real) (-> A B) (Listof (Listof B)))))
 (distinct (-> (Listof Any) Boolean))
 )

;; -----------------------------------------------------------------------------
;;bg; duplicated in state-adapted.rkt

(define-type Decisions (Listof (List Player (Listof (List Hotel Boolean)))))

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


;; ---------------------------------------------------------------------------------------------------
;; DATA: 
(define-type Player player)
(struct player (
  [name : String]
  [tiles : (Listof Tile)]
  [money : Cash]
  [shares : Shares]
  [external : (Option (Instance Player%))]
) #:transparent)
;; Player = (player String [Listof Tile] Amount Shares)
;; Amount = Nat
;; (player t a s) is the represetation of a player 
;; -- t is the list of tiles the player owns 
;; -- a is the amount of available money 
;; -- s are the shares owned 

(: player0 (-> String Tile Tile Tile Tile Tile Tile (Instance Player%) Player))
(define (player0 n t1 t2 t3 t4 t5 t6 x)
     (player n (list t1 t2 t3 t4 t5 t6) CASH0 player-shares0 x))

(: ext:*create-player (-> String Cash Shares (Listof Tile) Player))
(define (ext:*create-player name cash shares tiles)
  (unless (distinct tiles)
    (error '*create-player (format "Precondition: distinct tiles ~a" tiles)))
  (unless (<= STARTER-TILES# (length tiles))
    (error '*create-player (format "Precondition: <=~a tiles in ~a" STARTER-TILES# tiles)))
  (*create-player name cash shares tiles))

(: *create-player (-> String Cash Shares (Listof Tile) Player))
(define (*create-player name cash shares tiles)
  (player name tiles cash shares #f))

(: player-tile- (-> Player Tile Player))
(define (player-tile- p t)
  (struct-copy player p (tiles (remove t (player-tiles p)))))

(: player-tile+ (-> Player Tile Player))
(define (player-tile+ p t)
  (struct-copy player p (tiles (cons t (player-tiles p)))))

(: player-shares++ (-> Player Hotel * Player ))
(define (player-shares++ p . h)
  (if (empty? h)
      p
      (struct-copy player p
                   (shares (for/fold : Shares
                                     ((s : Shares (player-shares p)))
                                     ((h : Hotel (in-list h)))
                             (shares++ s h))))))


;; Player ShareOrder Board -> Player
(: player-buy-shares (-> Player (Listof Hotel) Board Player))
(define (player-buy-shares p0 sh board)
  (define amount (for/sum : Cash
                          ((h : Hotel sh))
                   ;;bg; handle failure silently
                   (or (price-per-share h (size-of-hotel board h)) 0)))
  (define m (assert (- (player-money p0) amount) exact-nonnegative-integer?))
  (apply player-shares++ (struct-copy player p0 (money m)) sh))

;; Player Shares Board -> Player
(: player-returns-shares (-> Player Shares Board Player))
(define (player-returns-shares p0 transfers board)
  (match-define (player _name _tiles money shares _ext) p0)
  (define amount (shares->money transfers board))
  (struct-copy player p0 (money (+ money amount)) (shares (shares-minus shares transfers))))

;; ---------------------------------------------------------------------------------------------------
(struct state (
  [board : Board]
  [players : (Listof Player)]
  [tiles : (Listof Tile)]
  [hotels : (Listof Hotel)]
  [shares : Shares]
  [bad : (Listof Player)]
) #:transparent)
(define-type State state)
;; State  = (state Board [Listof Player] [Listof Hotel] Shares [Listof Player])
;; (state b p t h s bad) is a representation of a game state: 
;; -- b is the current board 
;; -- p is the state of all the players 
;;    the list order determines the order of turns,
;;    it is the turn of the first player on the list
;; -- t is the list of available tiles 
;; -- h is the list of available hotels 
;; -- s is the list of available shares 
;; -- bad is the list of players gone bad 

;; ---------------------------------------------------------------------------------------------------
;; FUNCTIONS: placing a tile and all consequences 

(: ext:state0 (-> Player * State))
(define (ext:state0 . p)
  (unless (distinct (apply append (map player-tiles p)))
    (error 'state0 (format "Precondition: distinct tiles for players ~a" p)))
  (apply state0 p))

(: state0 (-> Player * State))
(define (state0 . p)
  (define tiles-owned-by-players (apply append (map player-tiles p)))
  (define tiles-in-pool (remove* tiles-owned-by-players ALL-TILES))
  (state (make-board) p tiles-in-pool ALL-HOTELS banker-shares0 '()))

(: state-sub-shares (-> State Shares State))
(define (state-sub-shares s bad-shares)
  (struct-copy state s (shares (shares-minus (state-shares s) bad-shares))))

(: ext:*cs0 (-> String * State))
(define (ext:*cs0 . names)
  (unless (distinct names)
    (error '*cs0 (format "Precondition: (distinct ~a)" names)))
  (unless (andmap (lambda ([s : String]) (<= (string-length s) 20)) names)
    (error 'cs0 (format "Precondition: strings with <=20 characters ~a" names)))
  (apply *cs0 names))
  
;; Player *-> State 
;; create players with given names, assigning random tiles from the full pool
(: *cs0 (-> String * State))
(define (*cs0 . names)
  (let loop : State
       ((names : (Listof String) names)
        (tiles : (Listof Tile) ALL-TILES)
        (players : (Listof Player) '()))
    (cond
      [(empty? names)
       (state (make-board) (reverse players) tiles ALL-HOTELS banker-shares0 '())]
      [else (define first-six (take tiles STARTER-TILES#))
            (define player1 (player (first names) first-six CASH0 player-shares0 #f))
            (loop (rest names) (drop tiles STARTER-TILES#) (cons player1 players))])))

(: ext:*create-state (-> Board (Listof Player) State))
(define (ext:*create-state b lp)
  (unless (shares-combinable? (map player-shares lp))
    (error 'create-state))
  (unless (distinct (apply append (board-tiles b) (map player-tiles lp)))
    (error 'create-state "precond"))
  (*create-state b lp))

;; Board [Listof Player] -> State
;; create a state by subtracting the tiles on the board and the players from the pool
;; subtracting the shares of the players from the full shares 
;; subtracting the placed hotels from the available hotels 
(: *create-state (-> Board (Listof Player) State))
(define (*create-state board players)
  (define players-shares
    (for/list : (Listof Shares)
              ([p : Player (in-list players)])
      (player-shares p)))
  (define remaining-shares 
    (for/fold : Shares
              ((remaining-shares banker-shares0))
              [(s : Shares (in-list players-shares))]
      (shares-minus remaining-shares s)))
  (define remaining-hotels 
    (for/list : (Listof Hotel)
              ((h : Hotel (in-list ALL-HOTELS))
               #:when (= (size-of-hotel board h) 0))
      h))
  (define remaining-tiles 
    (remove* (apply append (board-tiles board) (map player-tiles players)) ALL-TILES))
  (state board players remaining-tiles remaining-hotels remaining-shares '()))

(: ext:state-place-tile (->* (State Tile) ((Option Hotel)) State))
(define (ext:state-place-tile s tile (hotel #f))
  (unless (member tile (player-tiles (state-current-player s)))
    (error 'state-place-tile (format "Precondition: tile ~a belongs to player ~a" tile (state-current-player s))))
  (unless (not (eq? (what-kind-of-spot (state-board s) tile) 'IMPOSSIBLE))
    (error 'state-place-tile (format "Precondition: impossible position for ~a on board" tile)))
  (let ([spot-type (what-kind-of-spot (state-board s) tile)])
    (when hotel ;; i.e., hotel is not unsupplied
      (unless (memq spot-type '(FOUNDING MERGING))
        (error 'state-place-tile (format "Precondition: expected founding or merging spot for ~a" tile))))
    (let ([b (state-board s)]
          [hotels (state-hotels s)])
      (unless (if (and (eq? spot-type 'FOUNDING) (pair? hotels))
                  (and hotel (member hotel hotels))
                  #t)
        (error 'state-place-tile (format "Precondition: if spot is FOUNDING and hotels are available, ~a must be one of them" hotel))))
    (unless (if (eq? spot-type 'MERGING)
                (and hotel
                     (let-values ([(w _) (merging-which (state-board s) tile)])
                       (member hotel w)))
                #t)
      (error 'state-place-tile (format "Precondition:  if tile placement causes merger, hotel ~a must be given and an acquirer" hotel))))
  (state-place-tile s tile hotel))

;; State Tile [Hotel] -> State 
;; place the tile (in the possession of player 1) onto board
;; if founding, use hotel 
;; if merging, hotel is acquirer
(: state-place-tile (->* (State Tile) ((Option Hotel)) State))
(define (state-place-tile s tile (hotel #f))
  (match-define (state board players tiles hotels shares _bad) s)
  (define current (player-tile- (first players) tile))
  (define others (rest players))
  (define players-next (cons current others))
  (define tiles-next (remove tile tiles))
  (define spot (what-kind-of-spot board tile))
  (cond
    [(or (eq? SINGLETON spot) (eq? GROWING spot) (and (eq? FOUNDING spot) (not hotel)))
     (define new-board 
       (if (eq? GROWING spot)
           (grow-hotel board tile)
           (place-tile board tile)))
     (struct-copy state s (board new-board) (tiles tiles-next) (players players-next))]
    [(eq? FOUNDING spot)
     (define t 
       (struct-copy state s
                    (hotels (remove hotel hotels))
                    (tiles tiles-next)
                    (board (found-hotel board tile hotel))))
     (if (= (shares-available shares hotel) 0)
      (struct-copy state t (players players-next))
      (struct-copy state t
                   (shares (shares-- shares hotel))
                   (players (cons (player-shares++ current hotel) others))))]
    [(eq? MERGING spot)
     (define-values (w l) (merging-which board tile))
     (define acquired (append (remove hotel w) l))
     (define next-state
       (struct-copy state s
                    (board (merge-hotels board tile (or hotel (error 'hotel=#f))))
                    (hotels (append acquired hotels))
                    (tiles tiles-next)
                    (players players-next)))
     ;; now distribute the bonus
     (foldr (state-distribute-bonus board) next-state acquired)]
    [else (error 'condfailed)]))

;; [Listf Hotel Nat] State -> State 
;; distribute bonus for the acquired hotel
(: state-distribute-bonus (-> Board (-> Hotel State State)))
(define ((state-distribute-bonus board) acquired-hotel s)
  (define size-acquired (size-of-hotel board acquired-hotel))
  (define players (state-players s))
  (define selector (lambda ([p : Player]) (shares-available (player-shares p) acquired-hotel)))
  (define owners-of-acquired (filter (lambda ([p : Player]) (> (selector p) 0)) players))
  (define owners-of-acquired-sorted
    (sort owners-of-acquired (lambda ([x : Player] [y : Player]) (> (selector x) (selector y)))))
  (cond
    [(empty? owners-of-acquired-sorted) s]
    [else
     (define majority-minority ((inst aux:partition Player Player)
       owners-of-acquired-sorted selector (lambda ([x : Player]) x)))
     (define majority (first majority-minority))
     (define minority (if (empty? (rest majority-minority)) '() (second majority-minority)))
     (define majority-bonus (bonus 'majority acquired-hotel size-acquired))
     (define minority-bonus (bonus 'minority acquired-hotel size-acquired))
     (cond
       [(pair? (rest majority))
        ;; distribute the majority+minority bonus
        (define total-bonus (+ majority-bonus minority-bonus))
        (define bonus-per (quotient total-bonus (length majority)))
        (struct-copy state s (players (foldr (state-pay-out bonus-per) players majority)))]
       [(cons? minority)  ;; (empty? (rest majority))
        (define single-majority (first majority))
        (define majority-payed ((state-pay-out majority-bonus) single-majority players))
        (define bonus-per (quotient minority-bonus (length minority)))
        (struct-copy state s (players (foldr (state-pay-out bonus-per) majority-payed minority)))]
       [(empty? minority)  ;; (empty? (rest majority))
        (define single-majority (first majority))
        (struct-copy state s
                     (players ((state-pay-out majority-bonus) single-majority players)))])]))

(: state-pay-out (-> Cash (-> Player [Listof Player] [Listof Player])))
(define (state-pay-out bonus)
  ;; add cash to wallet of pay-to in players
  (lambda ([pay-to : Player] [players : (Listof Player)])
    (define the-name (player-name pay-to))
    (for/list : (Listof Player)
        ((p : Player (in-list players)))
      (match-define (player name _tiles money _shares _ext) p)
      (if (string=? name the-name) (struct-copy player p (money (+ money bonus))) p))))

;; ---------------------------------------------------------------------------------------------------
;; FUNCTIONS: external 

(: ext:state-move-tile (-> State Tile State))
(define (ext:state-move-tile s t)
  (unless (member t (state-tiles s))
    (error 'state-move-tile (format "Precondition: tile ~a must be a member of ~a" t (state-tiles s))))
  (state-move-tile s t))

(: state-move-tile (-> State Tile State))
(define (state-move-tile s t)
  (match-define (state _board players tiles _hotels _shares _bad) s)
  (struct-copy state s 
               (players (cons (player-tile+ (first players) t) (rest players)))
               (tiles (remove t tiles))))

(: state-next-turn (-> State State))
(define (state-next-turn s)
  (define players (state-players s))
  (struct-copy state s (players (append (rest players) (list (first players))))))

(: state-remove-current-player (-> State State))
(define (state-remove-current-player s)
  (define players (state-players s))
  (struct-copy state s (players (rest players)) (bad (cons (first players) (state-bad s)))))

(: state-eliminate (-> State (Listof Player) State))
(define (state-eliminate s ep)
  (struct-copy state s (players (remove* ep (state-players s))) (bad (append ep (state-bad s)))))

(: state-current-player (-> State Player))
(define (state-current-player s)
  (first (state-players s)))

(: ext:state-buy-shares (-> State (Listof Hotel) State))
(define (ext:state-buy-shares s sh)
  (unless (shares-order? sh)
    (error 'state-buy-shares "Preciondignos"))
  (unless (affordable? (state-board s)
                       sh
                       (player-money (state-current-player s)))
    (error 'state-buy-shares (format "Precondition: player ~a cannot afford to buy ~a" (state-current-player s) sh)))
  (unless (let ([banker-s-shares (state-shares s)])
            (shares-available? banker-s-shares sh))
    (error 'state-buy-shares (format "shares ~a are not available")))
  (state-buy-shares s sh))

(: state-buy-shares (-> State (Listof Hotel) State))
(define (state-buy-shares s sh)
  (match-define (state board players _tiles _hotels shares _bad) s)
  (struct-copy state s 
               (players (cons (player-buy-shares (first players) sh board) (rest players)))
               (shares (for/fold : Shares
                                 ((s : Shares shares))
                                 ((h : Hotel sh))
                         (shares-- s h)))))

(: ext:state-return-shares (->* [State (Listof (List Player (Listof (List Hotel Boolean))))] [Board] State))
(define (ext:state-return-shares s decisions (board (state-board s)))
  (unless (= (length (state-players s)) (length decisions))
    (error 'state-return-shares (format "Precondition: need same number of players and decisions")))
  (state-return-shares s decisions board))

(: state-return-shares (->* [State (Listof (List Player (Listof (List Hotel Boolean))))] [Board] State))
(define (state-return-shares s decisions (board (state-board s)))
  (for/fold : State
            ((s : State s))
            ((d : (List Player (Listof (List Hotel Boolean))) decisions))
    (state-return-shares/player s (first d) (second d) board)))

;; State Player [Listof [List Hotel Any]] -> State 
;; return player p shares to state s according to its decisions
(: state-return-shares/player (->* [State Player (Listof (List Hotel Boolean))] [Board] State))
(define (state-return-shares/player s p p-s-decisions (board (state-board s)))
  (define the-name (player-name p))
  (define player-s (player-shares p))
  (define transfers (shares-to-be-moved+their-value player-s p-s-decisions))
  (match-define (state _board players _tiles _hotels shares _bad) s)
  (define new-players 
    (for/list : (Listof Player)
              ((q : Player players)) 
      (if (string=? (player-name q) the-name)
          (player-returns-shares q transfers board)
          q)))
  (struct-copy state s (shares (shares-plus shares transfers)) (players new-players)))

;; Board Shares [Listof [List Hotel Boolean]] -> Shares 
;; determine how the shares that must be transfered from player-s-shares to banker-s-shares
(: shares-to-be-moved+their-value (-> Shares (Listof (List Hotel Boolean)) Shares))
(define (shares-to-be-moved+their-value player-s-shares decisions)
  (for/fold : Shares
            ((shares : Shares player-shares0))
            ((d : (List Hotel Boolean) decisions))
    (define hotel (first d))
    (cond
      [(second d) shares]
      [else (define available (shares-available player-s-shares hotel))
            (for/fold : Shares
                      ((shares : Shares shares))
                      ((n : Natural (in-range available)))
              (shares++ shares hotel))])))

(: state-score (-> State (Listof (List String Cash))))
(define (state-score s0)
  (define board (state-board s0))
  (define bonus (state-distribute-bonus board))
  (define state/bonus
    (foldr (lambda ([h : Hotel] [s : State])
             (if (= (size-of-hotel board h) 0) s (bonus h s)))
           s0 ALL-HOTELS))
  (define scores 
    (for/list : (Listof (List String Cash))
              ((p : Player (in-list (state-players state/bonus))))
      (match-define (player name _tiles money shares _external) p)
      (list name (+ money (shares->money shares board)))))
  (sort scores 
        (lambda ([p : (List String Cash)] [q : (List String Cash)])
          (or (> (second p) (second q)) 
              (and (= (second p) (second q)) (string<=? (first p) (first q)))))))

(: shares->money (-> Shares Board Cash))
(define (shares->money shares board)
  (assert
  (for/sum : Integer
           ([(hotel n) (in-hash shares)])
    (define size (size-of-hotel board hotel))
    (define price (price-per-share hotel size))
    (if (and (> size 0) price) (* price n) 0)) exact-nonnegative-integer?))

(: state-final? (-> State Boolean))
(define (state-final? s)
  (define board  (state-board s))
  (define-values (winner? founded safe)
    (for/fold : (Values Boolean (Listof Hotel) (Listof Hotel))
              ((winner? : Boolean #f)
               (founded : (Listof Hotel) '())
               (safe : (Listof Hotel) '()))
              ((h : Hotel ALL-HOTELS))
      (define s (size-of-hotel board h))
      (cond
        [(>= s SAFE#) (values (>= s FINAL#) (cons h founded) (cons h safe))]
        [(> s 0) (values winner? (cons h founded) safe)]
        [else (values winner? founded safe)])))
  (or winner? (and (cons? founded) (= (length founded) (length safe)))))
