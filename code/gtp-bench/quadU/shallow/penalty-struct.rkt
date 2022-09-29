#lang typed/racket/base/shallow

;; For wrap.rkt

(provide (struct-out $penalty))

;; =============================================================================

(struct $penalty
  ([hyphens : Natural]
   [width   : Float]
) #:transparent)

