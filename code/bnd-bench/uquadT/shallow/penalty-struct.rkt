#lang typed/racket/base/shallow

(provide (struct-out $penalty))

;; -----------------------------------------------------------------------------

(require
  "../base/core-types.rkt")

;; =============================================================================

(struct $penalty
  ([hyphens : Nonnegative-Integer][width : Value-Type]) #:transparent)

(define-type Value-Type Float)
