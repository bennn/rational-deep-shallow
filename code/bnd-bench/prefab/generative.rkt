#lang racket

(module aaa racket
  (struct foo (f))
  (define myf (foo 'bbb))
  (provide myf (struct-out foo)))

(module bbb typed/racket
  (require/typed
    (submod ".." aaa)
    (#:struct foo ((f : Symbol))))
  (: getf  (-> foo Symbol))
  (define (getf ff)
    (foo-f ff))
  (provide getf))

(module ccc typed/racket
  (require/typed
    (submod ".." aaa)
    (#:struct foo ((f : Symbol)))
    (myf foo))
  (require (submod ".." bbb))
  (getf myf))

(require 'ccc)

;; Error: incompatible struct type with same name

