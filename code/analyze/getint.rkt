#lang racket

;; Step 0: ag "interface for" b-BENCHMARK/ (boundary profile data)
;; Step 1: run this
;; Step 2: make a #hash((x . y) ....) in interface-for/BENCHMARK
;; Step 3: for each value V add exporting module (V . SRC)
;; Step 4: add adaptors to hash (ADAPT . SRC)

(define (print-all str*)
  (for-each writeln str*))

(define (clean-all str*)
  (sort (remove-duplicates str*) string<?))

(define (parseln str)
  (cadr (regexp-match #rx"interface for ([^)]+)\\)" str)))

(module+
  main
  (require racket/cmdline)
  (command-line
    #:args (fn)
    (print-all
      (clean-all
        (map parseln (file->lines fn))))))
