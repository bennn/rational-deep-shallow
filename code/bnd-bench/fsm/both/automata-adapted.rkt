#lang typed/racket/base

(define-type Probability Nonnegative-Real)
(define-type Population (cons Automaton* Automaton*))
(define-type Automaton* [Vectorof Automaton])
(define-type Payoff Nonnegative-Real)

(define-type State Natural)
(define-type Transition* [Vectorof Transition])
(define-type Transition [Vectorof State])

(require "benchmark-util.rkt")
(require/typed/check "automata.rkt"
 [#:opaque Automaton automaton?]
)

(provide
Automaton
Probability Population Automaton* Payoff)

(module* main typed/racket/base
  (require require-typed-check (submod ".."))
  (require/typed "automata.rkt"
   (automaton-payoff (-> Automaton Payoff))
   (defects (-> Payoff Automaton))
   (cooperates (-> Payoff Automaton))
   (tit-for-tat (-> Payoff Automaton))
   (grim-trigger (-> Payoff Automaton))
   (make-random-automaton
    (-> Natural Automaton))
   (match-pair
     (-> Automaton Automaton Natural (values Automaton Automaton)))
   (automaton-reset
    (-> Automaton Automaton))
   (clone
    (-> Automaton Automaton)))
  (provide (all-from-out (submod "..")) defects cooperates tit-for-tat grim-trigger match-pair automaton-reset clone make-random-automaton automaton-payoff))

(module* population typed/racket/base
  (require require-typed-check (submod ".."))
  (require/typed "automata.rkt"
   (automaton-payoff (-> Automaton Payoff))
   (defects (-> Payoff Automaton))
   (cooperates (-> Payoff Automaton))
   (tit-for-tat (-> Payoff Automaton))
   (grim-trigger (-> Payoff Automaton))
   (make-random-automaton
    (-> Natural Automaton))
   (match-pair
     (-> Automaton Automaton Natural (values Automaton Automaton)))
   (automaton-reset
    (-> Automaton Automaton))
   (clone
    (-> Automaton Automaton)))
  (provide (all-from-out (submod "..")) defects cooperates tit-for-tat grim-trigger match-pair automaton-reset clone make-random-automaton automaton-payoff))

