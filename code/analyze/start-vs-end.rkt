#lang racket

;; Compare start & end running times.
;; How much improvement did we get?

;; For all benchmarks, all strategies, all trails.

;; ---

(require
  (only-in "main.rkt" benchmark->pi cfg-id cfg-perf)
  file/glob)

(struct row (start-cfg end-status end-perf n-steps step*) #:prefab)

(define dir-name "sve")

(define (go fn)
  (init! fn)
  #;(hash! fn)
  (xy! fn)
  (void))

(define (hash! fn)
  (define out (fn->out fn))
  (define out2 (path-add-extension out #".hash" #"."))
  (unless (file-exists? out2)
    (printf "HASH ~s~n" (path->string (file-name-from-path fn)))
    (define vv* (file->value out))
    (define acc#
      (let* ((acc# (make-hash)))
        (for ((vv (in-list vv*))
              #:when (improved-row? vv))
          (define start-ms (third vv))
          (define end-ms (fourth vv))
          (define diff-key (rnd* (diff* start-ms end-ms)))
          (hash-add1! acc# diff-key))
        acc#))
    (with-output-to-file out2 (lambda () (pretty-write acc#)))
    (void)))

(define (xy! fn)
  (define out (fn->out fn))
  (define out3 (path-add-extension out #".xy" #"."))
  (unless (file-exists? out3)
    (printf "XY ~s~n" (path->string (file-name-from-path fn)))
    (define vv* (file->value out))
    (define acc*
     (filter values
      (for/list ((vv (in-list vv*)))
        (cond
         ((short-row? vv)
          (list (second vv) (second vv)))
         ((improved-row? vv)
          (list (third vv) (fourth vv)))
         (else #f)))))
    (with-output-to-file out3 (lambda () (pretty-write acc*)))
    (void)))

(define (improved-row? xx)
  (and (not (short-row? xx))
       (second xx)))

(define (short-row? xx)
  (null? (cddr xx)))

(define (hash-add1! h k)
  (hash-update! h k add1 (lambda () 0)))

(define (diff* start-ms end-ms)
  (if (< start-ms end-ms)
    (- (/ end-ms start-ms))
    (/ start-ms end-ms)))

(define (rnd* nn)
  (exact-round nn))

(define (fn->out fn)
  (define file-name (file-name-from-path fn))
  (build-path dir-name file-name))

(define (init! fn)
  (define file-name (file-name-from-path fn))
  (ensure-dir dir-name)
  (define out-name (fn->out fn))
  (unless (file-exists? out-name)
    (printf "INIT ~a~n" (path->string file-name))
    (define bm-name (fn->bm file-name))
    (define row* (file->value fn))
    (define pi (benchmark->pi bm-name))
    (define ovr
      (let ((uperf (cfg-perf (car pi))))
        (lambda (nn) (/ nn uperf))))
    (define pi#
      (for/hash ((kv (in-list pi))) (values (cfg-id kv) (cfg-perf kv))))
    (define TOO-SLOW 10)
    (define vv*
      (for/list ((rr (in-list row*)))
        (define c0 (row-start-cfg rr))
        (define c1 (row-end-cfg rr))
        (define end-perf (row-end-perf rr))
        (define worst-on-trail
          (trail-max pi# (cdr (row-step* rr)) bm-name))
        (cond
          ((equal? c0 c1)
            (list c0 end-perf))
          ((and (second worst-on-trail)
                (<= TOO-SLOW (ovr (second worst-on-trail))))
           (list c0 #f (config-perf pi# c0 bm-name) worst-on-trail))
          (else
            (list c0 c1 (config-perf pi# c0 bm-name) end-perf)))))
    (with-output-to-file out-name
      (lambda () (pretty-write vv*)))
    (void)))

(define (config-perf pi# str bm-name)
  (or
    (hash-ref pi# str #f)
    (raise-user-error 'config-perf "~a: cfg not found ~s" bm-name str)))

(define (trail-max pi trail* bm-name)
  (for/fold ((curr-cfg #f)
             (curr-max #f)
             #:result (list curr-cfg curr-max))
            ((cfg (in-list trail*)))
    (define next-perf (config-perf pi cfg bm-name))
    (if (or (not curr-max) (< curr-max next-perf))
      (values cfg next-perf)
      (values curr-cfg curr-max))))

(define (row-end-cfg rr)
  (define tt (row-step* rr))
  (last tt))

(define (fn->bm pp)
  (string->symbol
    (first (string-split (path->string pp) "-"))))

(define (ensure-dir pp)
  (unless (directory-exists? pp)
    (make-directory pp)))

(define (all-t1-files)
  (glob "img/*rktd"))

(module+
main
(require racket/cmdline)
(for-each go (all-t1-files)))

