#lang typed/racket

(require require-typed-check/unsafe)

(unsafe-require/typed/check "data.rkt"
  [#:struct posn ([x : Real]
                  [y : Real])]
  [#:struct block ([x : Real]
                   [y : Real]
                   [color : Color])]
  [#:struct tetra ([center : posn]
                   [blocks : (Listof Block)])]
  [#:struct world ([tetra : tetra]
                   [blocks : (Listof Block)])])

(define-type Color Symbol)
(define-type Posn posn)
(define-type Block block)
(define-type Tetra tetra)
(define-type World world)
(define-type BSet  (Listof Block))

(provide
 (struct-out posn)
 (struct-out block)
 (struct-out tetra)
 (struct-out world)
 Posn
 Block
 Tetra
 World
 Color
 BSet
 Color
 BSet)

(module* aux typed/racket/base
  (require (submod "..") require-typed-check)
  (require/typed/check "data.rkt"
    [posn-x (-> posn Real)]
    [posn-y (-> posn Real)]
    [block-x (-> block Real)]
    [block-y (-> block Real)]
    [block-color (-> block Color)]
    [tetra-center (-> tetra posn)]
    [tetra-blocks (-> tetra (Listof Block))]
    [world-tetra (-> world tetra)]
    [world-blocks (-> world (Listof Block))]
  )
  (provide (all-from-out (submod "..")))
  (provide posn-x posn-y
    block-x block-y block-color
    tetra-center tetra-blocks
    world-tetra world-blocks))

(module* block typed/racket/base
  (require (submod "..") require-typed-check)
  (require/typed/check "data.rkt"
    [posn-x (-> posn Real)]
    [posn-y (-> posn Real)]
    [block-x (-> block Real)]
    [block-y (-> block Real)]
    [block-color (-> block Color)]
    [tetra-center (-> tetra posn)]
    [tetra-blocks (-> tetra (Listof Block))]
    [world-tetra (-> world tetra)]
    [world-blocks (-> world (Listof Block))]
  )
  (provide (all-from-out (submod "..")))
  (provide posn-x posn-y
    block-x block-y block-color
    tetra-center tetra-blocks
    world-tetra world-blocks))

(module* bset typed/racket/base
  (require (submod "..") require-typed-check)
  (require/typed/check "data.rkt"
    [posn-x (-> posn Real)]
    [posn-y (-> posn Real)]
    [block-x (-> block Real)]
    [block-y (-> block Real)]
    [block-color (-> block Color)]
    [tetra-center (-> tetra posn)]
    [tetra-blocks (-> tetra (Listof Block))]
    [world-tetra (-> world tetra)]
    [world-blocks (-> world (Listof Block))]
  )
  (provide (all-from-out (submod "..")))
  (provide posn-x posn-y
    block-x block-y block-color
    tetra-center tetra-blocks
    world-tetra world-blocks))

(module* elim typed/racket/base
  (require (submod "..") require-typed-check)
  (require/typed/check "data.rkt"
    [posn-x (-> posn Real)]
    [posn-y (-> posn Real)]
    [block-x (-> block Real)]
    [block-y (-> block Real)]
    [block-color (-> block Color)]
    [tetra-center (-> tetra posn)]
    [tetra-blocks (-> tetra (Listof Block))]
    [world-tetra (-> world tetra)]
    [world-blocks (-> world (Listof Block))]
  )
  (provide (all-from-out (submod "..")))
  (provide posn-x posn-y
    block-x block-y block-color
    tetra-center tetra-blocks
    world-tetra world-blocks))

(module* main typed/racket/base
  (require (submod "..") require-typed-check)
  (require/typed/check "data.rkt"
    [posn-x (-> posn Real)]
    [posn-y (-> posn Real)]
    [block-x (-> block Real)]
    [block-y (-> block Real)]
    [block-color (-> block Color)]
    [tetra-center (-> tetra posn)]
    [tetra-blocks (-> tetra (Listof Block))]
    [world-tetra (-> world tetra)]
    [world-blocks (-> world (Listof Block))]
  )
  (provide (all-from-out (submod "..")))
  (provide posn-x posn-y
    block-x block-y block-color
    tetra-center tetra-blocks
    world-tetra world-blocks))

(module* tetras typed/racket/base
  (require (submod "..") require-typed-check)
  (require/typed/check "data.rkt"
    [posn-x (-> posn Real)]
    [posn-y (-> posn Real)]
    [block-x (-> block Real)]
    [block-y (-> block Real)]
    [block-color (-> block Color)]
    [tetra-center (-> tetra posn)]
    [tetra-blocks (-> tetra (Listof Block))]
    [world-tetra (-> world tetra)]
    [world-blocks (-> world (Listof Block))]
  )
  (provide (all-from-out (submod "..")))
  (provide posn-x posn-y
    block-x block-y block-color
    tetra-center tetra-blocks
    world-tetra world-blocks))

(module* world typed/racket/base
  (require (submod "..") require-typed-check)
  (require/typed/check "data.rkt"
    [posn-x (-> posn Real)]
    [posn-y (-> posn Real)]
    [block-x (-> block Real)]
    [block-y (-> block Real)]
    [block-color (-> block Color)]
    [tetra-center (-> tetra posn)]
    [tetra-blocks (-> tetra (Listof Block))]
    [world-tetra (-> world tetra)]
    [world-blocks (-> world (Listof Block))]
  )
  (provide (all-from-out (submod "..")))
  (provide posn-x posn-y
    block-x block-y block-color
    tetra-center tetra-blocks
    world-tetra world-blocks))


