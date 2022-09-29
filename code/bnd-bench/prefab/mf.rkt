#lang racket

(module u:server racket
  (struct foo (f) #:extra-constructor-name make-foo #:prefab #:mutable)
  (define myf (foo 'bbb))
  (provide myf (struct-out foo)))

;; ---

(module t:adaptor typed/racket
  (require/typed
    (submod ".." u:server)
    (#:opaque foo foo?))
  (provide foo))

(module t:client1 typed/racket
  (require (submod ".." t:adaptor))
  (require/typed
    (submod ".." u:server)
    (make-foo (-> Symbol foo))
    (foo-f (-> foo Symbol))
    (set-foo-f! (-> foo Symbol Void)))

  (: getf  (-> foo Symbol))
  (define (getf ff)
    (foo-f ff))

  (provide getf))

;; ---

(module t:client2 typed/racket
  (require (submod ".." t:adaptor))
  (let () (struct foo ((f : Symbol))) (void))
  (require/typed
    (submod ".." u:server)
    (make-foo (-> Symbol foo))
    (foo-f (-> foo Symbol))
    (set-foo-f! (-> foo Symbol Void))
    (myf foo))
  (require (submod ".." t:client1))

  (getf myf))

(require 't:client2)

