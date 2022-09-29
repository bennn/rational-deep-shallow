#lang typed/racket/base

(require
  require-typed-check
  "structs-adapted.rkt"
  "benv-adapted.rkt"
)

;; -- time.rkt
(define-type Value Closure)
(provide Value)

(module* ai typed/racket/base
(require
  require-typed-check
  "structs-adapted.rkt"
  "benv-adapted.rkt"
  (submod "..")
)

(require/typed/check "time.rkt"
  [time-zero Time]
  [k (Parameterof Natural)]
  [tick (-> Stx Time Time)]
  [alloc (-> Time (-> Var Addr))]
)

(provide
  time-zero
  k
  tick
  alloc
  (all-from-out (submod ".."))))

(module* denotable typed/racket/base
(require
  require-typed-check
  "structs-adapted.rkt"
  "benv-adapted.rkt"
  (submod "..")
)

(require/typed/check "time.rkt"
  [time-zero Time]
  [k (Parameterof Natural)]
  [tick (-> Stx Time Time)]
  [alloc (-> Time (-> Var Addr))]
)

(provide
  time-zero
  k
  tick
  alloc
  (all-from-out (submod ".."))))

(module* ui typed/racket/base
(require
  require-typed-check
  "structs-adapted.rkt"
  "benv-adapted.rkt"
  (submod "..")
)

(require/typed/check "time.rkt"
  [time-zero Time]
  [k (Parameterof Natural)]
  [tick (-> Stx Time Time)]
  [alloc (-> Time (-> Var Addr))]
)

(provide
  time-zero
  k
  tick
  alloc
  (all-from-out (submod ".."))))

;; =============================================================================
