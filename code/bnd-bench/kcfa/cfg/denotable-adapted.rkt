#lang typed/racket/base

(require
  require-typed-check
  "structs-adapted.rkt"
  "benv-adapted.rkt"
  "time-adapted.rkt"
)

(define-type Denotable (Setof Value))
(define-type Store (HashTable Addr Denotable))

(require/typed/check "denotable.rkt"
  (#:opaque State State?))

(provide
  Denotable
  Store
  State State?
)

;; ---

(module* ai typed/racket/base
(require
  require-typed-check
  "structs-adapted.rkt"
  "benv-adapted.rkt"
  "time-adapted.rkt"
  (submod "..")
)

(require/typed/check "denotable.rkt"
  [State-call (-> State Exp)]
  [State-benv (-> State BEnv)]
  [State-store (-> State Store)]
  [State-time (-> State Time)]
   [d-bot Denotable]
   [d-join (-> Denotable Denotable Denotable)]
   [empty-store Store]
   [store-lookup (-> Store Addr Denotable)]
   [store-update (-> Store Addr Denotable Store)]
   [store-update* (-> Store (Listof Addr) (Listof Denotable) Store)]
   [store-join (-> Store Store Store)])

(provide
(all-from-out (submod ".."))
  d-bot
  d-join
  empty-store
  store-lookup
  store-update
  store-update*
  store-join
))

(module* ui typed/racket/base
(require
  require-typed-check
  "structs-adapted.rkt"
  "benv-adapted.rkt"
  "time-adapted.rkt"
  (submod "..")
)

(require/typed/check "denotable.rkt"
  [State-call (-> State Exp)]
  [State-benv (-> State BEnv)]
  [State-store (-> State Store)]
  [State-time (-> State Time)]
   [d-bot Denotable]
   [d-join (-> Denotable Denotable Denotable)]
   [empty-store Store]
   [store-lookup (-> Store Addr Denotable)]
   [store-update (-> Store Addr Denotable Store)]
   [store-update* (-> Store (Listof Addr) (Listof Denotable) Store)]
   [store-join (-> Store Store Store)])

(provide
(all-from-out (submod ".."))
  d-bot
  d-join
  empty-store
  store-lookup
  store-update
  store-update*
  store-join
))

