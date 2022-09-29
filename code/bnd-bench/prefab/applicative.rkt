#lang racket

(module u:server racket
  (struct foo (f) #:prefab #:mutable)
  (define myf (foo 'bbb))
  (provide myf (struct-out foo)))

;; ---

(module t:client1 typed/racket
  (struct foo ((f : Symbol)) #:prefab)

  (require/typed
    (submod ".." u:server)
    ((foo a:foo) (-> Symbol foo))
    #;((foo? a:foo?) (-> Any Boolean : foo))
    ((foo-f a:foo-f) (-> foo Symbol))
    ((set-foo-f! a:set-foo-f!) (-> foo Symbol Void)))

  (: getf  (-> foo Symbol))
  (define (getf ff)
    (a:foo-f ff))

  (provide getf))

;; ---

(module t:client2 typed/racket
  (struct foo ((f : Symbol)) #:prefab)

  (require/typed
    (submod ".." u:server)
    ((foo a:foo) (-> Symbol foo))
    #;((foo? a:foo?) (-> Any Boolean : foo))
    ((foo-f a:foo-f) (-> foo Symbol))
    ((set-foo-f! a:set-foo-f!) (-> foo Symbol Void))
    ((myf a:myf) foo))

  (require (submod ".." t:client1))

  (getf a:myf))

(require 't:client2)
