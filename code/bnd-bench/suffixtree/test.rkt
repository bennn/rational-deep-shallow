#lang racket/base

;; Well okay, not obviously unsound here!

(module aaa typed/racket
 (struct foo ((a : Symbol)) #:mutable #:prefab)
 (define ff (foo 'bb))
 (provide ff (struct-out foo)))

(module bbb racket
  (require (submod ".." aaa))
  (set-foo-a! ff 42))

(require 'aaa)
(require 'bbb)

(foo-a ff)


