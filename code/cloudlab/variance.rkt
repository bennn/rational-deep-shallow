#lang racket/base

(require
racket/file
racket/pretty
gtp-util
  math/statistics)

(define (analyze-benchmark main-file . bak-file*)
(printf "analyze '~a' with backups ~a~n" main-file bak-file*)
(define main-n* (parse-file main-file))
(define bak-n** (map parse-file bak-file*))
(unless (null? bak-n**)
(define bad* (out-of-bounds main-n* bak-n**))
(printf " ~a out-of-bounds rows in backups~n" (length bad*))
(for-each (lambda (x) (displayln (map (lambda (y) (map rnd y)) x))) bad*)
(newline))
(printf " ~a median stddev~n" (rnd (median < (map cadr main-n*))))
(printf " ~a max stddev~n" (rnd (apply max (map cadr main-n*))))
(printf "~neverything else~n")
(pretty-print (map (lambda (x) (map rnd x)) main-n*)))

(define (parse-file ff)
(with-input-from-file
ff
(lambda ()
(read-line)
(for/list ((sv (in-lines)))
(define vv (cadr (string->value sv)))
(define n* (map time-string->cpu-time vv))
(list (mean n*) (stddev n*))))))

(define (out-of-bounds main-n* bak-n**)
  (let loop ([main* main-n*] [bak** bak-n**])
    (if (or (null? main*) (ormap null? bak**))
    '()
    (let ()
    (define n0 (car main*))
    (define n1* (map car bak**))
    (define bad
        (for/first ((n1 (in-list n1*)) #:when
          (oob n0 n1))
          n1))
    (if bad
    (cons (list n0 bad) (loop (cdr main*) (map cdr bak**)))
    (loop (cdr main*) (map cdr bak**)))))))

(define (oob n0 n1)
  (define n0-hi (+ (car n0) (cadr n0)))
  (define n0-lo (- (car n0) (cadr n0)))
  (define n1-hi (+ (car n1) (cadr n1)))
  (define n1-lo (- (car n1) (cadr n1)))
  (or (< n0-hi n1-lo) (< n1-hi n0-lo)))

(module+
main
(require racket/cmdline)
(command-line
#:args file*
(apply analyze-benchmark file*)))

