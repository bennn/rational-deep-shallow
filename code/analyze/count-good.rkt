#lang racket/base

;; Count trouble configurations:
;; - slow as-is
;; - slow after toggling all-D or all-S
;; How many? What % of total?

(require
  racket/pretty
  gtp-util
  (only-in "main.rkt" sym->char benchmark->pi overhead overhead-hi cfg-id
  cfg-perf string-swap* all-benchmarks str-toggle-D str-toggle-S))

;; ---

(define str-toggle-D toggle-D)
(define str-toggle-S toggle-S)

(define (go bm*)
  (pretty-write
    (for/list ((bm (in-list bm*)))
      (cons bm (count-bad (benchmark->pi bm))))))

(define (count-bad pi)
  (define T (overhead-hi))
  (define perf# (for/hash ((ci (in-list pi))) (values (cfg-id ci) (cfg-perf ci))))
  (for/fold ((num-good 0)
             (num-cfg 0)
             #:result (list num-good num-cfg))
            ((ci (in-list pi)))
    (define cc (cfg-id ci))
    (define fast?
      (<= (overhead
            (min (hash-ref perf# cc)
                 (hash-ref perf# (toggle-D cc))
                 (hash-ref perf# (toggle-S cc)))
            pi)
          T))
    (values (+ (if fast? 1 0) num-good)
            (+ 1 num-cfg))))

(module+
main
(require racket/cmdline)
(go all-benchmarks))

