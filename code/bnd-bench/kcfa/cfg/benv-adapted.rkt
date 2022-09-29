#lang typed/racket/base

(require
  require-typed-check
  "structs-adapted.rkt"
)

(require/typed/check "benv.rkt"
  [#:opaque Closure Closure?]
  [#:opaque Binding Binding?]
)

(provide
  Closure
  Closure?
  Binding
  Binding?
  BEnv
  Addr
  Time
)

(define-type BEnv (HashTable Var Addr))
(define-type Addr Binding)
(define-type Time (Listof Label))

;; =============================================================================

(module* ai typed/racket/base
(require
  require-typed-check
  "structs-adapted.rkt"
  (submod "..")
)
  (require/typed/check "benv.rkt"
  [Closure-lam (-> Closure Lam)]
  [Closure-benv (-> Closure BEnv)]
  [Binding-var (-> Binding Var)]
  [Binding-time (-> Binding Time)]
    (empty-benv BEnv)
    (benv-lookup (-> BEnv Var Addr))
    (benv-extend (-> BEnv Var Addr BEnv))
    (benv-extend* (-> BEnv (Listof Var) (Listof Addr) BEnv)))
  (provide (all-from-out (submod ".."))
    empty-benv
    benv-lookup
    benv-extend
    benv-extend*))

(module* denotable typed/racket/base
(require
  require-typed-check
  "structs-adapted.rkt"
  (submod "..")
)
  (require/typed/check "benv.rkt"
  [Closure-lam (-> Closure Lam)]
  [Closure-benv (-> Closure BEnv)]
  [Binding-var (-> Binding Var)]
  [Binding-time (-> Binding Time)]
    (empty-benv BEnv)
    (benv-lookup (-> BEnv Var Addr))
    (benv-extend (-> BEnv Var Addr BEnv))
    (benv-extend* (-> BEnv (Listof Var) (Listof Addr) BEnv)))
  (provide (all-from-out (submod ".."))
    empty-benv
    benv-lookup
    benv-extend
    benv-extend*))

(module* time typed/racket/base
(require
  require-typed-check
  "structs-adapted.rkt"
  (submod "..")
)
  (require/typed/check "benv.rkt"
  [Closure-lam (-> Closure Lam)]
  [Closure-benv (-> Closure BEnv)]
  [Binding-var (-> Binding Var)]
  [Binding-time (-> Binding Time)]
    (empty-benv BEnv)
    (benv-lookup (-> BEnv Var Addr))
    (benv-extend (-> BEnv Var Addr BEnv))
    (benv-extend* (-> BEnv (Listof Var) (Listof Addr) BEnv)))
  (provide (all-from-out (submod ".."))
    empty-benv
    benv-lookup
    benv-extend
    benv-extend*))

(module* ui typed/racket/base
(require
  require-typed-check
  "structs-adapted.rkt"
  (submod "..")
)
  (require/typed/check "benv.rkt"
  [Closure-lam (-> Closure Lam)]
  [Closure-benv (-> Closure BEnv)]
  [Binding-var (-> Binding Var)]
  [Binding-time (-> Binding Time)]
    (empty-benv BEnv)
    (benv-lookup (-> BEnv Var Addr))
    (benv-extend (-> BEnv Var Addr BEnv))
    (benv-extend* (-> BEnv (Listof Var) (Listof Addr) BEnv)))
  (provide (all-from-out (submod ".."))
    empty-benv
    benv-lookup
    benv-extend
    benv-extend*))
