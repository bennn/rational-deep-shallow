#lang typed/racket/base

(provide
  Indexes
  In-Indexes
  Weighted-Signal
  Drum-Symbol
  Pattern
  (struct-out Array)
  (struct-out Settable-Array)
  (struct-out Mutable-Array))

(require require-typed-check/unsafe)

(unsafe-require/typed/check "data.rkt"
  [#:struct Array ([shape : Indexes]
                   [size : Integer]
                   [strict? : (Boxof Boolean)]
                   [strict! : (-> Void)]
                   [unsafe-proc : (-> Indexes Float)])]
  [#:struct (Settable-Array Array) ([set-proc : (Indexes Float -> Void)])]
  [#:struct (Mutable-Array Settable-Array) ([data : (Vectorof Float)])])

(define-type Indexes (Vectorof Integer))
(define-type In-Indexes Indexes)

;; From mix: A Weighted-Signal is a (List (Array Float) Real)
(define-type Weighted-Signal (List Array Real))

;; drum patterns are simply lists with either O (bass drum), X (snare) or
;; #f (pause)
(define-type Drum-Symbol (U 'O 'X #f))
(define-type Pattern (Listof Drum-Symbol))

(module* array-broadcast typed/racket/base
  (require require-typed-check (submod ".."))
  (require/typed/check "data.rkt"
    [Array-shape (-> Array Indexes)]
    [Array-size (-> Array Integer)]
    [Array-strict? (-> Array (Boxof Boolean))]
    [Array-strict! (-> Array (-> Void))]
    [Array-unsafe-proc (-> Array (-> Indexes Float))]
    [Settable-Array-set-proc (-> Mutable-Array (Indexes Float -> Void))]
    [Mutable-Array-data (-> Mutable-Array (Vectorof Float))])
  (provide (all-from-out (submod ".."))
    Array-shape Array-size Array-strict?  Array-strict!  Array-unsafe-proc Settable-Array-set-proc Mutable-Array-data))

(module* array-struct typed/racket/base
  (require require-typed-check (submod ".."))
  (require/typed/check "data.rkt"
    [Array-shape (-> Array Indexes)]
    [Array-size (-> Array Integer)]
    [Array-strict? (-> Array (Boxof Boolean))]
    [Array-strict! (-> Array (-> Void))]
    [Array-unsafe-proc (-> Array (-> Indexes Float))]
    [Settable-Array-set-proc (-> Mutable-Array (Indexes Float -> Void))]
    [Mutable-Array-data (-> Mutable-Array (Vectorof Float))])
  (provide (all-from-out (submod ".."))
    Array-shape Array-size Array-strict?  Array-strict!  Array-unsafe-proc Settable-Array-set-proc Mutable-Array-data))

(module* array-transform typed/racket/base
  (require require-typed-check (submod ".."))
  (require/typed/check "data.rkt"
    [Array-shape (-> Array Indexes)]
    [Array-size (-> Array Integer)]
    [Array-strict? (-> Array (Boxof Boolean))]
    [Array-strict! (-> Array (-> Void))]
    [Array-unsafe-proc (-> Array (-> Indexes Float))]
    [Settable-Array-set-proc (-> Mutable-Array (Indexes Float -> Void))]
    [Mutable-Array-data (-> Mutable-Array (Vectorof Float))])
  (provide (all-from-out (submod ".."))
    Array-shape Array-size Array-strict?  Array-strict!  Array-unsafe-proc Settable-Array-set-proc Mutable-Array-data))

(module* array-utils typed/racket/base
  (require require-typed-check (submod ".."))
  (require/typed/check "data.rkt"
    [Array-shape (-> Array Indexes)]
    [Array-size (-> Array Integer)]
    [Array-strict? (-> Array (Boxof Boolean))]
    [Array-strict! (-> Array (-> Void))]
    [Array-unsafe-proc (-> Array (-> Indexes Float))]
    [Settable-Array-set-proc (-> Mutable-Array (Indexes Float -> Void))]
    [Mutable-Array-data (-> Mutable-Array (Vectorof Float))])
  (provide (all-from-out (submod ".."))
    Array-shape Array-size Array-strict?  Array-strict!  Array-unsafe-proc Settable-Array-set-proc Mutable-Array-data))

(module* drum typed/racket/base
  (require require-typed-check (submod ".."))
  (require/typed/check "data.rkt"
    [Array-shape (-> Array Indexes)]
    [Array-size (-> Array Integer)]
    [Array-strict? (-> Array (Boxof Boolean))]
    [Array-strict! (-> Array (-> Void))]
    [Array-unsafe-proc (-> Array (-> Indexes Float))]
    [Settable-Array-set-proc (-> Mutable-Array (Indexes Float -> Void))]
    [Mutable-Array-data (-> Mutable-Array (Vectorof Float))])
  (provide (all-from-out (submod ".."))
    Array-shape Array-size Array-strict?  Array-strict!  Array-unsafe-proc Settable-Array-set-proc Mutable-Array-data))

(module* main typed/racket/base
  (require require-typed-check (submod ".."))
  (require/typed/check "data.rkt"
    [Array-shape (-> Array Indexes)]
    [Array-size (-> Array Integer)]
    [Array-strict? (-> Array (Boxof Boolean))]
    [Array-strict! (-> Array (-> Void))]
    [Array-unsafe-proc (-> Array (-> Indexes Float))]
    [Settable-Array-set-proc (-> Mutable-Array (Indexes Float -> Void))]
    [Mutable-Array-data (-> Mutable-Array (Vectorof Float))])
  (provide (all-from-out (submod ".."))
    Array-shape Array-size Array-strict?  Array-strict!  Array-unsafe-proc Settable-Array-set-proc Mutable-Array-data))

(module* mixer typed/racket/base
  (require require-typed-check (submod ".."))
  (require/typed/check "data.rkt"
    [Array-shape (-> Array Indexes)]
    [Array-size (-> Array Integer)]
    [Array-strict? (-> Array (Boxof Boolean))]
    [Array-strict! (-> Array (-> Void))]
    [Array-unsafe-proc (-> Array (-> Indexes Float))]
    [Settable-Array-set-proc (-> Mutable-Array (Indexes Float -> Void))]
    [Mutable-Array-data (-> Mutable-Array (Vectorof Float))])
  (provide (all-from-out (submod ".."))
    Array-shape Array-size Array-strict?  Array-strict!  Array-unsafe-proc Settable-Array-set-proc Mutable-Array-data))

(module* sequencer typed/racket/base
  (require require-typed-check (submod ".."))
  (require/typed/check "data.rkt"
    [Array-shape (-> Array Indexes)]
    [Array-size (-> Array Integer)]
    [Array-strict? (-> Array (Boxof Boolean))]
    [Array-strict! (-> Array (-> Void))]
    [Array-unsafe-proc (-> Array (-> Indexes Float))]
    [Settable-Array-set-proc (-> Mutable-Array (Indexes Float -> Void))]
    [Mutable-Array-data (-> Mutable-Array (Vectorof Float))])
  (provide (all-from-out (submod ".."))
    Array-shape Array-size Array-strict?  Array-strict!  Array-unsafe-proc Settable-Array-set-proc Mutable-Array-data))

(module* synth typed/racket/base
  (require require-typed-check (submod ".."))
  (require/typed/check "data.rkt"
    [Array-shape (-> Array Indexes)]
    [Array-size (-> Array Integer)]
    [Array-strict? (-> Array (Boxof Boolean))]
    [Array-strict! (-> Array (-> Void))]
    [Array-unsafe-proc (-> Array (-> Indexes Float))]
    [Settable-Array-set-proc (-> Mutable-Array (Indexes Float -> Void))]
    [Mutable-Array-data (-> Mutable-Array (Vectorof Float))])
  (provide (all-from-out (submod ".."))
    Array-shape Array-size Array-strict?  Array-strict!  Array-unsafe-proc Settable-Array-set-proc Mutable-Array-data))


