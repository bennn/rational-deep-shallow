#lang typed/racket

(provide
 Automaton
 oAutomaton
 Payoff
)

(require require-typed-check)

(define-type Payoff Nonnegative-Real)
(define-type Transition* [Vectorof [Vectorof State]])
(define-type State Natural)
(define-type Input Natural)
(define-type Automaton
    (Class
     (init-field [current State]
                 [payoff Payoff] 
                 [table Transition*] 
                 [original State #:optional])
     [match-pair
      ;; the sum of pay-offs for the two respective automata over all rounds
      (-> oAutomaton Natural (values oAutomaton oAutomaton))]
     [jump
      ;; this has no business being public 
      (-> State Payoff Void)]
     [pay
      (-> Payoff)]
     [reset
      ;; reset the historic payoff 
      (-> oAutomaton)]
     [clone
      ;; reset payoff and current state to original strategy
      (-> oAutomaton)]
     [equal (-> oAutomaton Boolean)]))
(define-type oAutomaton (Instance Automaton))

(module* main typed/racket/base
  (require (submod "..") require-typed-check)
  (require/typed/check "automata.rkt"
   (make-random-automaton
    (-> Natural oAutomaton)))
  (provide make-random-automaton (all-from-out (submod ".."))))

(module* population typed/racket/base
  (require (submod "..") require-typed-check)
  (require/typed/check "automata.rkt"
   (make-random-automaton
    (-> Natural oAutomaton)))
  (provide make-random-automaton (all-from-out (submod ".."))))
