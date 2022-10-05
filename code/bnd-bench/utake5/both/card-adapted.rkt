#lang typed/racket/base

(provide
  Card
  card
  card-face
  card-bulls)

(require
  require-typed-check/unsafe
  "basics-types.rkt")
(unsafe-require/typed/check "card.rkt"
 (#:struct card (
  [face : Face]
  [bulls : Bulls])))

(define-type Card card)

(module* card-pool typed/racket/base
  (require (submod "..") require-typed-check "basics-types.rkt")
  (require/typed/check "card.rkt"
   (card-face (-> Card Face))
   (card-bulls (-> Card Bulls))
   (>-face (-> Card Card Boolean))
   (--face (-> Card Card Natural)))
  (provide (all-from-out (submod "..")) card-face card-bulls >-face --face))

(module* dealer typed/racket/base
  (require (submod "..") require-typed-check "basics-types.rkt")
  (require/typed/check "card.rkt"
   (card-face (-> Card Face))
   (card-bulls (-> Card Bulls))
   (>-face (-> Card Card Boolean))
   (--face (-> Card Card Natural)))
  (provide (all-from-out (submod "..")) card-face card-bulls >-face --face))

(module* deck typed/racket/base
  (require (submod "..") require-typed-check "basics-types.rkt")
  (require/typed/check "card.rkt"
   (card-face (-> Card Face))
   (card-bulls (-> Card Bulls))
   (>-face (-> Card Card Boolean))
   (--face (-> Card Card Natural)))
  (provide (all-from-out (submod "..")) card-face card-bulls >-face --face))

(module* player typed/racket/base
  (require (submod "..") require-typed-check "basics-types.rkt")
  (require/typed/check "card.rkt"
   (card-face (-> Card Face))
   (card-bulls (-> Card Bulls))
   (>-face (-> Card Card Boolean))
   (--face (-> Card Card Natural)))
  (provide (all-from-out (submod "..")) card-face card-bulls >-face --face))

(module* stack typed/racket/base
  (require (submod "..") require-typed-check "basics-types.rkt")
  (require/typed/check "card.rkt"
   (card-face (-> Card Face))
   (card-bulls (-> Card Bulls))
   (>-face (-> Card Card Boolean))
   (--face (-> Card Card Natural)))
  (provide (all-from-out (submod "..")) card-face card-bulls >-face --face))

