#lang racket

(struct row (start-cfg end-status end-perf n-steps step*) #:prefab) 
(struct trail (start-cfg end-status end-perf n-steps step*) #:prefab) 

(define (parse fn)
  (file->value fn))

(define (clean row*)
  (define ums (row-end-perf (car row*)))
  (define (overhead n) (exact-ceiling (/ n ums)))
  (for/list ((rr (in-list row*)))
    (trail
      (row-start-cfg rr)
      `(overhead ,(overhead (row-end-perf rr)))
      `(steps ,(row-n-steps rr))
      (row-end-status rr)
      (row-step* rr))))

(define (order xx)
  (sort xx > #:key (compose1 cadr trail-end-status)))

(module+ main
  (require racket/cmdline)
  (command-line
    #:args (fn)
    (pretty-print-columns 123)
    (pretty-write (order (clean (parse fn))))))
