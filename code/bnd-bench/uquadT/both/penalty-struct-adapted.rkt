#lang typed/racket/base

(provide (struct-out $penalty))

;; -----------------------------------------------------------------------------

(require
  require-typed-check/unsafe
  "../base/core-types.rkt")

(unsafe-require/typed/check "penalty-struct.rkt"
  [#:struct $penalty ([hyphens : Nonnegative-Integer]
                      [width : Value-Type])])

;; =============================================================================

(define-type Value-Type Float)

(module* wrap typed/racket/base
  (require (submod "..") require-typed-check)
(define-type Value-Type Float)
  (require/typed/check "penalty-struct.rkt"
    ($penalty-hyphens (-> $penalty Nonnegative-Integer))
    ($penalty-width (-> $penalty Value-Type)))
  (provide (all-from-out (submod "..")) $penalty-width $penalty-hyphens))

