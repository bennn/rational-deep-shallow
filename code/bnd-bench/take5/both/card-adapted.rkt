#lang typed/racket/base

(provide
  Card
  card
  card-face
  card-bulls)

(require
  require-typed-check
  "basics-types.rkt")
(require/typed/check "card.rkt"
 (#:struct card (
  [face : Face]
  [bulls : Bulls])))

(define-type Card card)

(module* card-pool typed/racket/base
  (require (submod "..") require-typed-check "basics-types.rkt")
  (require/typed/check "card.rkt"
   (>-face (-> Card Card Boolean))
   (--face (-> Card Card Natural)))
  (provide (all-from-out (submod "..")) >-face --face))

(module* dealer typed/racket/base
  (require (submod "..") require-typed-check "basics-types.rkt")
  (require/typed/check "card.rkt"
   (>-face (-> Card Card Boolean))
   (--face (-> Card Card Natural)))
  (provide (all-from-out (submod "..")) >-face --face))

(module* deck typed/racket/base
  (require (submod "..") require-typed-check "basics-types.rkt")
  (require/typed/check "card.rkt"
   (>-face (-> Card Card Boolean))
   (--face (-> Card Card Natural)))
  (provide (all-from-out (submod "..")) >-face --face))

(module* player typed/racket/base
  (require (submod "..") require-typed-check "basics-types.rkt")
  (require/typed/check "card.rkt"
   (>-face (-> Card Card Boolean))
   (--face (-> Card Card Natural)))
  (provide (all-from-out (submod "..")) >-face --face))

(module* stack typed/racket/base
  (require (submod "..") require-typed-check "basics-types.rkt")
  (require/typed/check "card.rkt"
   (>-face (-> Card Card Boolean))
   (--face (-> Card Card Natural)))
  (provide (all-from-out (submod "..")) >-face --face))

