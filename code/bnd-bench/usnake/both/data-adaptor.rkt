#lang typed/racket

(require require-typed-check/unsafe)

(unsafe-require/typed/check "data.rkt"
  [#:struct posn ([x : Real]
                  [y : Real])]
  [#:struct snake ([dir : Dir]
                   [segs : (NEListof Posn)])]
  [#:struct world ([snake : Snake]
                   [food  : Posn])])

(define-type (NEListof A) (Pairof A (Listof A)))
(define-type Dir (U "up" "down" "left" "right"))
(define-type Snake snake)
(define-type World world)
(define-type Posn  posn)

(provide
 (struct-out posn)
 (struct-out snake)
 (struct-out world)
 Dir
 Snake
 World
 Posn
 NEListof)


(module* collide typed/racket
  (require (submod "..") require-typed-check)
  (require/typed/check "data.rkt"
    [posn-x (-> posn Real)]
    [posn-y (-> posn Real)]
    [snake-dir (-> snake Dir)]
    [snake-segs (-> snake (NEListof Posn))]
    [world-snake (-> world Snake)]
    [world-food  (-> world Posn)])
  (provide (all-from-out (submod ".."))
    posn-x posn-y snake-dir snake-segs world-snake world-food))

(module* const typed/racket
  (require (submod "..") require-typed-check)
  (require/typed/check "data.rkt"
    [posn-x (-> posn Real)]
    [posn-y (-> posn Real)]
    [snake-dir (-> snake Dir)]
    [snake-segs (-> snake (NEListof Posn))]
    [world-snake (-> world Snake)]
    [world-food  (-> world Posn)])
  (provide (all-from-out (submod ".."))
    posn-x posn-y snake-dir snake-segs world-snake world-food))

(module* cut-tail typed/racket
  (require (submod "..") require-typed-check)
  (require/typed/check "data.rkt"
    [posn-x (-> posn Real)]
    [posn-y (-> posn Real)]
    [snake-dir (-> snake Dir)]
    [snake-segs (-> snake (NEListof Posn))]
    [world-snake (-> world Snake)]
    [world-food  (-> world Posn)])
  (provide (all-from-out (submod ".."))
    posn-x posn-y snake-dir snake-segs world-snake world-food))

(module* handlers typed/racket
  (require (submod "..") require-typed-check)
  (require/typed/check "data.rkt"
    [posn-x (-> posn Real)]
    [posn-y (-> posn Real)]
    [snake-dir (-> snake Dir)]
    [snake-segs (-> snake (NEListof Posn))]
    [world-snake (-> world Snake)]
    [world-food  (-> world Posn)])
  (provide (all-from-out (submod ".."))
    posn-x posn-y snake-dir snake-segs world-snake world-food))

(module* main typed/racket
  (require (submod "..") require-typed-check)
  (require/typed/check "data.rkt"
    [posn-x (-> posn Real)]
    [posn-y (-> posn Real)]
    [snake-dir (-> snake Dir)]
    [snake-segs (-> snake (NEListof Posn))]
    [world-snake (-> world Snake)]
    [world-food  (-> world Posn)])
  (provide (all-from-out (submod ".."))
    posn-x posn-y snake-dir snake-segs world-snake world-food))

(module* motion-help typed/racket
  (require (submod "..") require-typed-check)
  (require/typed/check "data.rkt"
    [posn-x (-> posn Real)]
    [posn-y (-> posn Real)]
    [snake-dir (-> snake Dir)]
    [snake-segs (-> snake (NEListof Posn))]
    [world-snake (-> world Snake)]
    [world-food  (-> world Posn)])
  (provide (all-from-out (submod ".."))
    posn-x posn-y snake-dir snake-segs world-snake world-food))

(module* motion typed/racket
  (require (submod "..") require-typed-check)
  (require/typed/check "data.rkt"
    [posn-x (-> posn Real)]
    [posn-y (-> posn Real)]
    [snake-dir (-> snake Dir)]
    [snake-segs (-> snake (NEListof Posn))]
    [world-snake (-> world Snake)]
    [world-food  (-> world Posn)])
  (provide (all-from-out (submod ".."))
    posn-x posn-y snake-dir snake-segs world-snake world-food))

