#lang racket

;; TODO lattice pict from gtp

;; ---

(require
  "main.rkt"
  math/statistics
)

;; ---

(define (go bm-name)
  (define pi (benchmark->pi bm-name))
  (define num-configs (length pi))
  (define uperf (cfg-perf (car pi)))
  (define (uovr x) (overhead (cfg-perf x) uperf))
  (define-values [fast-cfgs other-cfgs]
    (partition (lambda (x) (<= (uovr x) (overhead-hi))) pi))
  (define num-fast (length fast-cfgs))
  (define other-perf (map uovr other-cfgs))
  (printf "~a~n" bm-name)
  (printf " ~a configs, ~a under 1x, ~a over 1x, ~a mean ~a median~n"
          num-configs num-fast (length other-cfgs) (mean other-perf) (median < other-perf))
  (define pi-lvl (group-level pi))
  (pretty-write (init-lattice pi-lvl))
  (pretty-write (step1-lattice pi-lvl))
  (void)
  (void))

(define (group-level pi)
  (let* ((lvl* (group-by (compose1 cfg->fingerprint cfg-id) pi)))
    (sort lvl* < #:key (compose1 cfg->num-types cfg-id car))))

(define (cannonize str)
  (apply
  string
  (for/list ((c (in-string str)))
    (if (eq? c #\0) c #\1))))

(define (cfg->fingerprint str)
 (apply string
  (for/list ((cc (in-string str)))
    (if (eq? #\0 cc) cc #\1))))

(define (cfg->num-types str)
  (for/sum ((cc (in-string str))
            #:unless (eq? #\0 cc))
    1))

(define (init-lattice pi-lvl)
  (define u-perf (cfg-perf (caar pi-lvl)))
  (define (uoverhead t) (overhead t u-perf))
  (for/list ((lvl (in-list pi-lvl)))
    (define t* (map (compose1 uoverhead cfg-perf) lvl))
    (list
      (cannonize (cfg-id (car lvl)))
      (length lvl)
      (apply min t*)
      (apply max t*))))

(define (step1-lattice pi-lvl)
  (define u-perf (cfg-perf (caar pi-lvl)))
  (define (uoverhead t) (overhead (cfg-perf t) u-perf))
  (define all-trouble* (box '()))
  (define vv
    (for/list ((lvl (in-list pi-lvl)))
      (define trivial-level?
        (let* ((untyped-lvl? (and (null? (cdr lvl)) (untyped-config? (caar lvl))))
               (deep-good? (or untyped-lvl? (<= (uoverhead (find-deep lvl)) (overhead-hi))))
               (ds-good? (or deep-good? (<= (uoverhead (find-shallow lvl)) (overhead-hi)))))
          ds-good?))
      (define trouble*
        (if trivial-level?
          '()
          (for/list ((cfg (in-list lvl))
                     #:unless (<= (uoverhead cfg) (overhead-hi)))
            (uoverhead cfg))))
      (set-box! all-trouble* (append trouble* (unbox all-trouble*)))
      (list
        (cannonize (cfg-id (car lvl)))
        (length trouble*)
        (and (not (null? trouble*)) (apply min trouble*))
        (and (not (null? trouble*)) (apply max trouble*)))))
  (define at* (unbox all-trouble*))
  (printf "end: ~a trouble configs, ~a mean, ~a median~n" (length at*) (mean at*) (median < at*))
  vv)

(define (find-deep lvl)
  (or (for/first ((cfg (in-list lvl))
          #:when (deep-config? (cfg-id cfg)))
          cfg)
      (raise-arguments-error 'find-deep "no deep config" "ids" (map cfg-id lvl))))

(define (find-shallow lvl)
  (or (for/first ((cfg (in-list lvl))
                  #:when (shallow-config? (cfg-id cfg)))
          cfg)
      (raise-arguments-error 'find-shallow "no shallow config" "ids" (map cfg-id lvl))))

(module+ main (go 'fsm))

