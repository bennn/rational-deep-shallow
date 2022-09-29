#lang typed/racket

;; 2x => identifier already defined

;(: f0 (-> Any Any))
;(: f0? (-> Any Any))
(define-values [f0 f0?]
  (let ()
    (struct foo ((i : Real)))
    (values foo foo?)))

;(: f1 (-> Any Any))
;(: f1? (-> Any Any))
;(define-values [f1 f1?]
;  (let ()
;    (struct foo ((i : Real)))
;    (values foo foo?)))

(define x0 (f0 0))
;(define x1 (f1 1))
;
;x0 x1
;
;(f0? x0)
;(f1? x0)
;
;(f0? x1)
;(f1? x1)
;
