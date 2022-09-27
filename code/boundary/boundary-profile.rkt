#lang racket/base

;; boundary profile
;;
;; Like the contract profiler, but group samples by their module boundary
;; (from the blame object) and report the highest-cost boundary

(require racket/list
         racket/match
         racket/format
         racket/set
         (only-in racket/contract/combinator
                  blame-swapped? blame-swap blame-context
                  blame-contract blame-positive blame-negative blame-source
                  blame-missing-party? blame-add-missing-party
                  contract-continuation-mark-key)

         profile/sampler
         profile/utils
         ;profile/analyzer

         contract-profile/utils
         profile/raco-utils
         (for-syntax racket/base syntax/parse))

(define current-sample-rate (make-parameter 0.005))

(define-syntax (boundary-profile/user stx)
  (syntax-parse stx
    [(_ body:expr ...)
     #`(let ([sampler (create-sampler
                       (current-thread)
                       (current-sample-rate)
                       (current-custodian)
                       (list contract-continuation-mark-key))])
         (begin0 (begin body ...)
           (let ()
             (sampler 'stop)
             (define samples (sampler 'get-snapshots))
             (define contract-samples
               (for/list ([s (in-list (sampler 'get-custom-snapshots))])
                 (and (not (empty? s)) (car s))))
             (analyze-contract-samples contract-samples samples))))]))

(define (analyze-contract-samples contract-samples samples)
  (define correlated (correlate-contract-samples contract-samples samples))
  (print-breakdown correlated)
  (void))

(define (correlate-contract-samples contract-samples time+samples)
  ;; car of time+samples is total time, car of each sample is thread id
  ;; for now, we just assume a single thread. fix this eventually.
  (define total-time (car time+samples))
  ;; reverse is there to sort samples in forward time, which get-times needs.
  (define samples
    (for/list ([s (in-list (get-times (map cdr (reverse (cdr time+samples)))))])
      (cons (real->double-flonum (car s)) (cdr s))))
  ;; combine blame info and stack trace info. samples should line up
  (define live-contract-samples
    ;; If the sampler was stopped after recording a contract sample, but
    ;; before recording the corresponding time sample, the two lists may
    ;; be of different lengths. That's ok, just drop the extra sample.
    (for/list ([c-s (in-list contract-samples)]
               [s   (in-list samples)]
               #:when c-s)
      (match-define `#(,-blame) c-s)
      ;; In some cases, blame information is missing a party, in which.
      ;; case the contract system provides a pair of the incomplete blame
      ;; and the missing party. We combine the two here.
      (define blame
        (if (pair? -blame)
            (blame-add-missing-party (car -blame) (cdr -blame))
            -blame))
      (contract-sample blame #f s)))
  (define all-blames
    (set->list (for/set ([c-s (in-list live-contract-samples)])
                 (define b (contract-sample-blame c-s))
                 ;; all blames must be complete, otherwise we get bogus profiles
                 (when (blame-missing-party? b)
                   (error (string-append "contract-profile: incomplete blame:\n"
                                         (format-blame b))))
                 ;; An original blamed and its swapped version are the same
                 ;; for our purposes.
                 (if (blame-swapped? b)
                     (blame-swap b) ; swap back
                     b))))
  (contract-profile total-time live-contract-samples all-blames #f))

(define (print-breakdown correlated [show-by-caller? #f])
  (match-define (contract-profile total-time live-contract-samples all-blames _p) correlated)
  (define total-contract-time (samples-time live-contract-samples))
  (define contract-ratio (/. total-contract-time total-time))
  (printf "Running time is ~a% contracts\n"
          (~r (* 100 contract-ratio) #:precision 2))
  (printf "~a/~a ms\n\n"
          (~r total-contract-time #:precision 0)
          total-time)
  (define shorten-source
    (make-srcloc-shortener all-blames blame-source))
  (define (format-samples-time s)
    (define total-time (samples-time s))
    (format "~a ms" (~r total-time #:precision 2)))
  (define (print-boundary/samples b s)
    (printf "~s~n  ~a~n" b (format-samples-time s)))
  (define (contract-sample->boundary x)
    (define b (contract-sample-blame x))
    (append
      (sort
        (map
          ~a
          (list (blame-positive b) (blame-negative b)))
        string<?)
      (list
        (srcloc->string (shorten-source b))
        )))
  (define samples-by-boundary
    (sort (group-by (lambda (x)
                      (contract-sample->boundary x)
                      #;(blame-contract (contract-sample-blame x)))
                    live-contract-samples)
          >
          #:key samples-time
          #:cache-keys? #t))
  (define location-width 65)
  (for ([g (in-list samples-by-boundary)])
    (define boundary (contract-sample->boundary (car g)))
    (print-boundary/samples boundary g)
    (for ((x (in-list (remove-duplicates (map (compose1 blame-contract contract-sample-blame) g)))))
      (printf "  ~a~n" x))
    (void))
  (void))

(define (/. num den) (/ num (max den 1) 1.0))

;; ---

(define (boundary-profile file out-file)
  (with-output-to-file
    out-file
    #:exists 'replace
    (lambda ()
      (boundary-profile/user
       (dynamic-require (module-to-profile file) #f)))))

(module+ main
  (require racket/cmdline)
  (command-line
    #:args (main-file)
    (boundary-profile main-file "boundary-profile.txt")))


