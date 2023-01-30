#lang racket/base

;; TODO
;; - [ ] build / run simple versions (profile + boundary)
;; - [ ] improve the profile parsing ... deep vs shallow how to compute time
;; - [ ] ...?

;; analyze = run a rational programmer experiment on a benchmark
;;
;; inputs:
;; - 3d perf lattice, 
;; - 3d profile lattice
;; - 3d boundary profile lattice
;; - ? aux. benchmark info
;;
;; outputs:
;; - table with [[ start# status end-perf N-steps path ]]
;; - histogram of end overhead
;; - success proportion
;; - histogram of num steps

(require
  file/glob
  racket/pretty
  racket/string
  racket/list
  racket/math
  racket/file
  racket/path
  racket/port
  plot/no-gui
  pict pict-abbrevs
  (only-in math/statistics mean)
  (only-in gtp-util rnd string->value time-string->cpu-time)
)

;; ---

(define overhead-hi 2)
(define types-lo 2)

(define twoshot-deep-too-slow 12) ;; overhead vs untyped

(define (done-cfg? cfg overhead)
  (and (< types-lo (cfg-count-typed cfg)) (<= overhead overhead-hi)))

(define warning# (make-hash))

(define (warning-set! k v)
  (hash-set! warning# k v))

(define (print-warnings)
  (unless (hash-empty? warning#)
    (for-each
      (lambda (ss) (printf "WARNING: ~a" ss))
      (map cdr (sort (hash->list warning#) string<? #:key car)))))

(define (cfg-count-typed cc)
(for/sum ((c (in-string cc)) #:unless (eq? c #\0 )) 1))

(define out-dir "img")

(define key:bm 'benchmark)
(define key:prf 'prf_total)
(define key:prf2 'prf_self)
(define key:bnd 'boundary)
(define key:pi 'perf)
(define key:strategy 'strategy)

(struct row (start-cfg end-status end-perf n-steps step*) #:prefab)
(struct profile-sample (idx total% self% name+src) #:prefab)

(define (profile-node-src nn)
  (hash-ref nn 'src))

;; idx added by Ben, see parse-profile-data:json
(define (profile-node-idx nn)
  (hash-ref nn 'idx))

(define (profile-node-modname nn)
  (define src (profile-node-src nn))
  (if src
    (name+src->modname src)
    (raise-arguments-error 'profile-node-modname "expected string source" "src" src "node idx" (profile-node-idx nn) "node" nn)))

(define (profile-node-self-ms nn)
  (hash-ref nn 'self))

(define (profile-node-total-ms nn)
  (hash-ref nn 'total))

(define (prf#-nodes prf#)
  (hash-ref prf# 'nodes))

(define (prf#-cpu-time prf)
  (hash-ref prf 'cpu_time))

(define cfg-id first)
(define cfg-perf second)

(define (run-full-experiment bm-name strategy)
  (define pi (benchmark->pi bm-name))
  (define prf-dir (benchmark->profile-dir bm-name))
  (define bnd-dir (benchmark->boundary-dir bm-name))
  (hash
    key:bm bm-name
    key:strategy strategy
    key:pi pi
    key:prf2 (run-profile bm-name pi prf-dir #:S strategy #:P 'self)
    key:prf (run-profile bm-name pi prf-dir #:S strategy)
    key:bnd (run-boundary bm-name pi bnd-dir #:S strategy)))

(define (benchmark->pi bm-name)
; #lang gtp-measure/output/deep-shallow-untyped
; ("0000000" ("cpu time: 1063 real time: 1063 gc time: 91" "cpu time: 1076 real time: 1076 gc time: 93" "cpu time: 1174 real time: 1174 gc time: 101" "cpu time: 1067 real time: 1067 gc time: 92" "cpu time: 1088 real time: 1088 gc time: 102" "cpu time: 1084 real time: 1084 gc time: 91" "cpu time: 1087 real time: 1087 gc time: 91" "cpu time: 1096 real time: 1096 gc time: 97"))
; ("0000001" ("cpu time: 2593 real time: 2593 gc time: 111" "cpu time: 2657 real time: 2657 gc time: 100" "cpu time: 2634 real time: 2634 gc time: 112" "cpu time: 2588 real time: 2588 gc time: 100" "cpu time: 2584 real time: 2584 gc time: 110" "cpu time: 2659 real time: 2660 gc time: 100" "cpu time: 2633 real time: 2633 gc time: 105" "cpu time: 2560 real time: 2560 gc time: 105"))
; ("0000002" ("cpu time: 2840 real time: 2841 gc time: 129" "cpu time: 3040 real time: 3040 gc time: 129" "cpu time: 2883 real time: 2883 gc time: 115" "cpu time: 2860 real time: 2860 gc time: 128" "cpu time: 2912 real time: 2912 gc time: 116" "cpu time: 2825 real time: 2825 gc time: 121" "cpu time: 2855 real time: 2855 gc time: 127" "cpu time: 2840 real time: 2840 gc time: 127"))
; ("0000010" ("cpu time: 1075 real time: 1075 gc time: 99" "cpu time: 1067 real time: 1067 gc time: 89" "cpu time: 1069 real time: 1069 gc time: 89" "cpu time: 1093 real time: 1093 gc time: 99" "cpu time: 1063 real time: 1063 gc time: 97" "cpu time: 1092 real time: 1092 gc time: 89" "cpu time: 1081 real time: 1082 gc time: 99" "cpu time: 1074 real time: 1074 gc time: 89"))
; ("0000020" ("cpu time: 1098 real time: 1098 gc time: 120" "cpu time: 1097 real time: 1097 gc time: 121" "cpu time: 1083 real time: 1083 gc time: 106" "cpu time: 1104 real time: 1104 gc time: 107" "cpu time: 1085 real time: 1085 gc time: 106" "cpu time: 1112 real time: 1112 gc time: 120" "cpu time: 1102 real time: 1102 gc time: 113" "cpu time: 1080 real time: 1080 gc time: 107"))
  (define fn (glob1 (format "../../data/runtime/*-~a.out" bm-name)))
  (with-input-from-file
    fn
    (lambda ()
      (void (read-line))
      (for/list ((ln (in-lines))
                 (ii (in-naturals 1)))
        (define vv
          (with-handlers ((exn:fail? (lambda (xx) (printf "exn parsing ~s line ~s~n" fn ii) (raise xx))))
            (string->value ln)))
        (define id (car vv))
        (define tt* (map time-string->cpu-time (cadr vv)))
        (list id (mean tt*))))))

(define (glob1 pat)
  (define mm (glob pat))
  (cond [(null? mm) (raise-arguments-error 'glob1 "no matches" "pattern" pat)]
  [(not (null? (cdr mm))) (raise-arguments-error 'glob1 "+1 matches" "pattern" pat "matches" mm)]
  [else (car mm)]))

(define (pi-perf pi cfg)
  (for/first ((ci (in-list pi))
  #:when (equal? (cfg-id ci) cfg))
  (cfg-perf ci)))

(define (overhead pp pi)
  (/ pp (if (number? pi) pi (cfg-perf (pi-untyped-cfg pi)))))

(define (pi-untyped-cfg pi)
  (for/first ((ci (in-list pi)) #:when (for/and ((x (in-string (cfg-id ci)))) (untyped-bit? x)))
  ci))

(define (benchmark->profile-dir bm-name)
  (benchmark->x-dir bm-name "profile"))

(define (benchmark->boundary-dir bm-name)
  (benchmark->x-dir bm-name "boundary"))

(define (benchmark->x-dir bm-name str)
  (define u-name (string-append "u" bm-name))
  (or (find-data-dir str u-name)
      (find-data-dir str bm-name)
      (raise-arguments-error
        (string->symbol (format "benchmark->~a-dir" str))
        (format "cannot find ~s dir" str)
        "benchmark" bm-name
        "inferred path" (format-data-dir str bm-name))))

(define (find-data-dir str bm-name)
  (define dd (format-data-dir str bm-name))
  (and (directory-exists? dd) dd))

(define (format-data-dir str bm-name)
  (format "../../data/~a/~a/" str bm-name))

(define (benchmark->home-dir bm-name)
  (define dd (format "../gtp-bench/~a/" bm-name))
  (if (directory-exists? dd)
    dd
    (raise-arguments-error 'benchmark->home-dir
    "cannot find home dir"
    "benchmark" bm-name
    "inferred path" dd)))

(define (run-profile bm-name pi prf-dir #:S strategy #:P [pmode 'total])
  (define s-next
    (case strategy
     ((greedy) prf:json:greedy-find-next)
     ((busy) prf:busy-find-next)
     ((twoson) prf:twoson-find-next)
     (else (raise-argument-error 'run-boundary "strategy?" strategy))))
  (run-exp bm-name pi (s-next bm-name pi prf-dir #:P pmode)))

(define (run-boundary bm-name pi bnd-dir #:S strategy)
  (define s-next
    (case strategy
     ((greedy) bnd:greedy-find-next)
     ((busy) bnd:busy-find-next)
     ((twoson) bnd:twoson-find-next)
     (else (raise-argument-error 'run-boundary "strategy?" strategy))))
  (run-exp bm-name pi (s-next bm-name pi bnd-dir)))

;; find-next : (-> TODO (values Status string?))
;;  Status = (or/c TODO)

(define (prf:twoson-find-next bm-name pi prf-dir #:P [pmode 'total])
  (define (cfg->prf# str) (parse-profile-data:json (build-path prf-dir str)))
  (define mod* (module-names bm-name))
  (define (mod->idx str)
    (define ii (index-of mod* str))
    (if (exact-nonnegative-integer? ii) ii (raise-arguments-error 'mod->idx "module not found" "name" str "modules" mod*)))
  (define ms-key
    (case pmode
     ((total)  profile-node-total-ms)
     ((self) profile-node-self-ms)
     (else (raise-argument-error 'prf:json:greedy-find-next "profile-mode?" pmode))))
  (define (node-typed? cfg nn)
    (define name (profile-node-modname nn))
    (define idx (mod->idx name))
    (if (or (deep-module? cfg idx) (shallow-module? cfg idx))
      1 0))
  (define TST '(D S))
  (define (state0?)
    (and (pair? TST) (eq? 'D (car TST))))
  (define (state1?)
    (and (pair? TST) (eq? 'S (car TST))))
  (define (state2?)
    (null? TST))
  (define (next-state!)
    (set! TST (cdr TST)))
  (define foverhead
    (let ((u-perf (prf#-cpu-time (cfg->prf# (make-string (length mod*) #\0)))))
      (lambda (prf)
        (define cpu (prf#-cpu-time prf))
        (/ cpu u-perf))))
  (lambda (cfg)
    ;; Rank top profile points by types then by `pmode` ms
    ;; Pick top non-Deep, upgrade it
    (let* ((prf# (cfg->prf# cfg))
           (node* (prf#-nodes prf#))
           (node* (filter
                    (lambda (nn)
                      (define src (profile-node-src nn))
                      (define filename
                        (let ((ff (and (string? src) (srcloc-filename? src))))
                          (and ff (cadr ff))))
                      (and (member filename mod*)
                           (not (node-typed? cfg nn))
                           filename))
                    node*))
           (node* (sort node* > #:key ms-key))
           (name* (map profile-node-modname node*)))
      (cond
      [(and (state0?) (< twoshot-deep-too-slow (foverhead prf#)))
       ;; switch all D to S
       (next-state!)
       (values '(success D*=>S*) (string-swap* cfg #\1 #\2))]
      [(null? name*)
       (cond
        [(state1?)
         (next-state!)
         (values '(success S*=>D*) (string-swap* cfg #\2 #\1))]
        [else
         (values '(error no-mods) #f)])]
      [else
       (let ((r
         (for*/first ((m (in-list name*)) (idx (in-value (mod->idx m))))
           (if (state1?)
             (list '(success U->S) (string-update cfg idx #\2))
             (list '(success U->D) (string-update cfg idx #\1))))))
           (if r
           (apply values r)
        (values '(error no-untyped-mods) #f)))]))))

(define (prf:busy-find-next bm-name pi prf-dir #:P [pmode 'total])
  (define (cfg->prf# str) (parse-profile-data:json (build-path prf-dir str)))
  (define mod* (module-names bm-name))
  (define (mod->idx str)
    (define ii (index-of mod* str))
    (if (exact-nonnegative-integer? ii) ii (raise-arguments-error 'mod->idx "module not found" "name" str "modules" mod*)))
  (define ms-key
    (case pmode
     ((total)  profile-node-total-ms)
     ((self) profile-node-self-ms)
     (else (raise-argument-error 'prf:json:busy-find-next "profile-mode?" pmode))))
  (define (node-typed? cfg nn)
    (define name (profile-node-modname nn))
    (define idx (mod->idx name))
    (if (or (deep-module? cfg idx) (shallow-module? cfg idx))
      1 0))
  (lambda (cfg)
    ;; Rank top profile points by types then by `pmode` ms
    ;; Pick top non-Deep, upgrade it
    (let* ((prf# (cfg->prf# cfg))
           (node* (hash-ref prf# 'nodes))
           (node* (filter
                    (lambda (nn)
                      (define src (profile-node-src nn))
                      (define filename
                        (let ((ff (and (string? src) (srcloc-filename? src))))
                          (and ff (cadr ff))))
                      (and (member filename mod*) filename))
                    node*))
           (node* (sort node* >> #:key (lambda (x) (list (node-typed? cfg x) (ms-key x)))))
           (name* (map profile-node-modname node*)))
      (cond
      [(null? name*)
       (values '(error no-mods) #f)]
      [else
       (let ((r
         (for*/first ((m (in-list name*)) (idx (in-value (mod->idx m)))
                             #:unless (deep-module? cfg idx))
           (if (shallow-module? cfg idx)
             (list '(success S->D) (string-update cfg idx #\1))
             (list '(success U->S) (string-update cfg idx #\2)))
           )))
           (if r
           (apply values r)
        (values '(error no-typed-mods) #f)))]))))

;; json = jsexpr
(define (prf:json:greedy-find-next bm-name pi prf-dir #:P [pmode 'total])
  (define (cfg->prf# str) (parse-profile-data:json (build-path prf-dir str)))
  (define mod* (module-names bm-name))
  (define (mod->idx str)
    (define ii (index-of mod* str))
    (if (exact-nonnegative-integer? ii) ii (raise-arguments-error 'mod->idx "module not found" "name" str "modules" mod*)))
  (define ms-key
    (case pmode
     ((total)  profile-node-total-ms)
     ((self) profile-node-self-ms)
     (else (raise-argument-error 'prf:json:greedy-find-next "profile-mode?" pmode))))
  (lambda (cfg)
    ;; Rank top profile points by `pmode` ms (total or self)
    ;; Pick first non-Deep one, upgrade it
    (let* ((prf# (cfg->prf# cfg))
           (node* (hash-ref prf# 'nodes))
           (node* (filter
                    (lambda (nn)
                      (define src (profile-node-src nn))
                      (define filename
                        (let ((ff (and (string? src) (srcloc-filename? src))))
                          (and ff (cadr ff))))
                      (and (member filename mod*) filename))
                    node*))
           (node* (sort node* > #:key ms-key))
           (name* (map profile-node-modname node*)))
      (cond
      [(null? name*)
       (values '(error no-mods) #f)]
      [else
       (let ((r
         (for*/first ((m (in-list name*)) (idx (in-value (mod->idx m)))
                             #:unless (deep-module? cfg idx))
           (if (shallow-module? cfg idx)
             (list '(success S->D) (string-update cfg idx #\1))
             (list '(success U->S) (string-update cfg idx #\2)))
           )))
           (if r
           (apply values r)
        (values '(error no-typed-mods) #f)))]))))

(define (prf:text:greedy-find-next bm-name pi prf-dir)
  (define (cfg->prf str) (parse-profile-data:text (build-path prf-dir str)))
  (define mod* (module-names bm-name))
  (define (mod->idx str)
    (define ii (index-of mod* str))
    (if (exact-nonnegative-integer? ii) ii (raise-arguments-error 'mod->idx "module not found" "name" str "modules" mod*)))
  (lambda (cfg)
    ;; Rank top profile points by Total ms
    ;; Pick first non-Deep one, upgrade it
    (let* ((row* (cfg->prf cfg))
           (row* (filter (lambda (rr) (string-contains? (profile-sample-name+src rr) bm-name))
                   row*))
           (row* (sort row* > #:key profile-sample-self%))
           (mod* (map (compose1 name+src->modname profile-sample-name+src) row*)))
      (cond
      [(null? mod*)
       (values '(error no-mods) #f)]
      [else
       (let ((r
         (for*/first ((m (in-list mod*)) (idx (in-value (mod->idx m)))
                             #:unless (deep-module? cfg idx))
           (if (shallow-module? cfg idx)
             (list '(success S->D) (string-update cfg idx #\1))
             (list '(success U->S) (string-update cfg idx #\2)))
           )))
           (if r
           (apply values r)
        (values '(error no-typed-mods) #f)))]))))

(define (name+src->modname str)
  (define mm
    (or (regexp-match #rx"cfg/?([^.]+\\.rkt)" str)
        (regexp-match #rx"^([^/.]+\\.rkt)" str)))
  (if mm
    (cadr mm)
    (raise-argument-error 'name+src->modname "parse error in module name" str)))

(define (string-swap* str a b)
  (apply string
    (for/list ((cc (in-string str)))
      (if (eq? cc a) b cc))))

(define (string-update str idx c)
  (apply string
    (for/list ((cc (in-string str)) (jj (in-naturals))) (if (eqv? idx jj) c cc))))

(define (interface-resolver bm-name)
  (define int-path (build-path "interface-for" bm-name))
  (cond
   ((file-exists? int-path)
    (with-handlers ((exn:fail? (lambda (xx) (printf "exn parsing ~s~n" int-path) (raise xx))))
      (file->value int-path)))
   (else
    (raise-argument-error 'interface-resolver "unknown benchmark" bm-name))))

(define (bnd:greedy-find-next bm-name pi bnd-dir)
  (define (cfg->bnd str) (parse-bnd-data (build-path bnd-dir str)))
  (define mod* (module-names bm-name))
  (define (mod->idx str)
    (define ii (index-of mod* str))
    (if (exact-nonnegative-integer? ii) ii (raise-arguments-error 'mod->idx "module not found" "name" str "modules" mod*)))
  (define blame:infer-modname
    (let* ((ii# (interface-resolver bm-name))
           (interface->modname (lambda (k) (hash-ref ii# k k))))
      (lambda (str)
        (define fn
          (cond
           [(ignore-filename? str)
            #f]
           [(interface-for? str)
            => (compose1 interface->modname cadr)]
           [(submod-path? str)
            ;; case must come after interface-for?
            => cadr]
           [(srcloc-filename? str)
            => (compose1 interface->modname cadr)]
           [else
            ;; resolve "denotable-adapted.rkt" etc.
            (let* ((ff (path->string (file-name-from-path str)))
                   (ss (srcloc-filename? ff))
                   (ff (if ss (cadr ss) ff)))
              (interface->modname ff))]))
         (if (or (not fn) (member fn mod*))
           fn
           (#;raise-arguments-error
            begin
            (warning-set!
              str
              (format "resolved to a module name that does not exist~n ~a: ~a~n ~a: ~a~n ~a: ~a~n"
               "original" str
               "modname" fn
               "expected one of" mod*))
             #f)))))
  (lambda (cfg)
    ;; Parse the file, pick the worst valid boundary, then upgrade
    ;; Upgrade plan:
    ;; - U-D ==> D-D
    ;; - U-S ==> S-S
    ;; - S-D ==> D-D
    (let* ((bnd* (cfg->bnd cfg))
           (bnd* (filter values
                   (for/list ((bb (in-list bnd*)))
                     (define blm (bnd->blame bb))
                     (define mods
                       (let* ((mm (map blame:infer-modname blm))
                              (mm (filter values mm))
                              (mm (remove-duplicates mm)))
                         (cond
                          [(null? mm) ;; OK
                           ;; TODO #false or null? do we want the null later?
                           #f]
                          [(null? (cdr mm))
                           (if (blame-whitelist? bm-name mm blm)
                             #f
                             (begin #;raise-arguments-error
                              #;(printf
                               "WARNING ~a ~a~n ~a: ~a~n ~a: ~a~n ~a: ~a~n ~a: ~a~n"
                               'bnd:greedy
                               "malformed bnd profile output, only one module name"
                               "mods" mm "original" blm "context" bb "benchmark" bm-name)
                               #f))]
                          [(not (null? (cdr (cdr mm))))
                           (raise-arguments-error 'bnd:greedy
                             "malformed bnd profile output, +2 module names"
                             "mods" mm "original" blm "context" bb "benchmark" bm-name)]
                          [else
                           mm])))
                     (and mods (list mods (bnd->ms bb) (bnd->ctc bb))))))
           (bnd* (sort bnd* > #:key bnd->ms)))
      (cond
      [(null? bnd*)
       (values '(error no-mods) #f)]
      [else
       (let ((r
         (for/or ((bnd (in-list bnd*)))
           (define mod-info*
             (sort
               (for/list ((mm (in-list (bnd->blame bnd))))
                 (define idx (mod->idx mm))
                 (define knd (string-ref cfg idx))
                 (list knd idx mm))
               char<?
               #:key car))
           (define key (map car mod-info*))
           (cond
            ;; U-D / 0-1 => 1-1
            ;; U-S / 0-2 => 2-2
            ;; D-S / 1-2 => 1-1
            [(equal? key '(#\0 #\1))
             (list '(success U-D->D-D) (string-update cfg (cadr (car mod-info*)) #\1))]
            [(equal? key '(#\0 #\2))
             (list '(success U-S->S-S) (string-update cfg (cadr (car mod-info*)) #\2))]
            [(equal? key '(#\1 #\2))
             (list '(success D-S->D-D) (string-update cfg (cadr (cadr mod-info*)) #\1))]
            [else
             (raise-arguments-error 'bnd:greedy
               "unknown key / boundary"
               "key" key
               "boundary info" mod-info*
               "cfg" cfg
               "bm" bm-name)])))
           )
           (if r
           (apply values r)
        (values '(error no-actionable) #f)))]))))

(define (bnd:busy-find-next bm-name pi bnd-dir)
  (define (cfg->bnd str) (parse-bnd-data (build-path bnd-dir str)))
  (define mod* (module-names bm-name))
  (define (mod->idx str)
    (define ii (index-of mod* str))
    (if (exact-nonnegative-integer? ii) ii (raise-arguments-error 'mod->idx "module not found" "name" str "modules" mod*)))
  (define (mod-typed? cfg mod)
    (define idx (mod->idx mod))
    (or (deep-module? cfg idx)
      (shallow-module? cfg idx)))
  (define blame:infer-modname
    (let* ((ii# (interface-resolver bm-name))
           (interface->modname (lambda (k) (hash-ref ii# k k))))
      (lambda (str)
        (define fn
          (cond
           [(ignore-filename? str)
            #f]
           [(interface-for? str)
            => (compose1 interface->modname cadr)]
           [(submod-path? str)
            ;; case must come after interface-for?
            => cadr]
           [(srcloc-filename? str)
            => (compose1 interface->modname cadr)]
           [else
            ;; resolve "denotable-adapted.rkt" etc.
            (let* ((ff (path->string (file-name-from-path str)))
                   (ss (srcloc-filename? ff))
                   (ff (if ss (cadr ss) ff)))
              (interface->modname ff))]))
         (if (or (not fn) (member fn mod*))
           fn
           (#;raise-arguments-error
            begin
            (warning-set!
              str
              (format "resolved to a module name that does not exist~n ~a: ~a~n ~a: ~a~n ~a: ~a~n"
               "original" str
               "modname" fn
               "expected one of" mod*))
             #f)))))
  (lambda (cfg)
    ;; Parse the file, pick the worst valid boundary, then upgrade
    ;; Upgrade plan:
    ;; - U-D ==> D-D
    ;; - U-S ==> S-S
    ;; - S-D ==> D-D
    (let* ((bnd* (cfg->bnd cfg))
           (bnd* (filter values
                   (for/list ((bb (in-list bnd*)))
                     (define blm (bnd->blame bb))
                     (define mods
                       (let* ((mm (map blame:infer-modname blm))
                              (mm (filter values mm))
                              (mm (remove-duplicates mm)))
                         (cond
                          [(null? mm) ;; OK
                           ;; TODO #false or null? do we want the null later?
                           #f]
                          [(null? (cdr mm))
                           (if (blame-whitelist? bm-name mm blm)
                             #f
                             (begin #;raise-arguments-error
                              #;(printf
                               "WARNING ~a ~a~n ~a: ~a~n ~a: ~a~n ~a: ~a~n ~a: ~a~n"
                               'bnd:busy
                               "malformed bnd profile output, only one module name"
                               "mods" mm "original" blm "context" bb "benchmark" bm-name)
                               #f))]
                          [(not (null? (cdr (cdr mm))))
                           (raise-arguments-error 'bnd:busy
                             "malformed bnd profile output, +2 module names"
                             "mods" mm "original" blm "context" bb "benchmark" bm-name)]
                          [else
                           mm])))
                     (and mods (list mods (bnd->ms bb) (bnd->ctc bb))))))
           (bnd* (sort bnd* >> #:key (lambda (x)
                                       (define mods (map blame:infer-modname (bnd->blame x)))
                                       (list (for/sum ((bb (in-list (bnd->blame x))))
                                               (define mm (blame:infer-modname bb))
                                               (define tt (mod-typed? cfg mm))
                                               (if tt 1 0))
                                             (bnd->ms x))))))
      (cond
      [(null? bnd*)
       (values '(error no-mods) #f)]
      [else
       (let ((r
         (for/or ((bnd (in-list bnd*)))
           (define mod-info*
             (sort
               (for/list ((mm (in-list (bnd->blame bnd))))
                 (define idx (mod->idx mm))
                 (define knd (string-ref cfg idx))
                 (list knd idx mm))
               char<?
               #:key car))
           (define key (map car mod-info*))
           (cond
            ;; DS => DD / 21 => 11
            ;; UD => SD / 01 => 21
            ;; US => UD / 02 => 01
            [(equal? key '(#\1 #\2))
             (list '(success DS->DD) (string-update cfg (cadr (cadr mod-info*)) #\1))]
            [(equal? key '(#\0 #\1))
             (list '(success UD->SD) (string-update cfg (cadr (car mod-info*)) #\2))]
            [(equal? key '(#\0 #\2))
             (list '(success US->UD) (string-update cfg (cadr (cadr mod-info*)) #\1))]
            [else
             (raise-arguments-error 'bnd:busy
               "unknown key / boundary"
               "key" key
               "boundary info" mod-info*
               "cfg" cfg
               "bm" bm-name)])))
           )
           (if r
           (apply values r)
        (values '(error no-actionable) #f)))]))))

(define (bnd:twoson-find-next bm-name pi bnd-dir)
  (define (cfg->bnd str) (parse-bnd-data (build-path bnd-dir str)))
  (define (bnd-cpu-time str)
    (bndpath-cpu-time (build-path bnd-dir str)))
  (define mod* (module-names bm-name))
  (define (mod->idx str)
    (define ii (index-of mod* str))
    (if (exact-nonnegative-integer? ii) ii (raise-arguments-error 'mod->idx "module not found" "name" str "modules" mod*)))
  (define blame:infer-modname
    (let* ((ii# (interface-resolver bm-name))
           (interface->modname (lambda (k) (hash-ref ii# k k))))
      (lambda (str)
        (define fn
          (cond
           [(ignore-filename? str)
            #f]
           [(interface-for? str)
            => (compose1 interface->modname cadr)]
           [(submod-path? str)
            ;; case must come after interface-for?
            => cadr]
           [(srcloc-filename? str)
            => (compose1 interface->modname cadr)]
           [else
            ;; resolve "denotable-adapted.rkt" etc.
            (let* ((ff (path->string (file-name-from-path str)))
                   (ss (srcloc-filename? ff))
                   (ff (if ss (cadr ss) ff)))
              (interface->modname ff))]))
         (if (or (not fn) (member fn mod*))
           fn
           (#;raise-arguments-error
            begin
            (warning-set!
              str
              (format "resolved to a module name that does not exist~n ~a: ~a~n ~a: ~a~n ~a: ~a~n"
               "original" str
               "modname" fn
               "expected one of" mod*))
             #f)))))
  (define TST '(D S))
  (define (state0?)
    (and (pair? TST) (eq? 'D (car TST))))
  (define (state1?)
    (and (pair? TST) (eq? 'S (car TST))))
  (define (state2?)
    (null? TST))
  (define (next-state!)
    (set! TST (cdr TST)))
  (define foverhead
  ;; TODO
    (let ((u-perf (bnd-cpu-time (make-string (length mod*) #\0))))
      (lambda (cfg)
        (define cpu (bnd-cpu-time cfg))
        (/ cpu u-perf))))
  (lambda (cfg)
    ;; Parse the file, pick the worst valid boundary, then upgrade
    ;; Upgrade plan:
    ;; - U-D ==> D-D
    ;; - U-S ==> S-S
    ;; - S-D ==> D-D
    (let* ((bnd* (cfg->bnd cfg))
           (bnd* (filter values
                   (for/list ((bb (in-list bnd*)))
                     (define blm (bnd->blame bb))
                     (define mods
                       (let* ((mm (map blame:infer-modname blm))
                              (mm (filter values mm))
                              (mm (remove-duplicates mm)))
                         (cond
                          [(null? mm) ;; OK
                           ;; TODO #false or null? do we want the null later?
                           #f]
                          [(null? (cdr mm))
                           (if (blame-whitelist? bm-name mm blm)
                             #f
                             (begin #;raise-arguments-error
                              #;(printf
                               "WARNING ~a ~a~n ~a: ~a~n ~a: ~a~n ~a: ~a~n ~a: ~a~n"
                               'bnd:greedy
                               "malformed bnd profile output, only one module name"
                               "mods" mm "original" blm "context" bb "benchmark" bm-name)
                               #f))]
                          [(not (null? (cdr (cdr mm))))
                           (raise-arguments-error 'bnd:greedy
                             "malformed bnd profile output, +2 module names"
                             "mods" mm "original" blm "context" bb "benchmark" bm-name)]
                          [else
                           mm])))
                     (and mods (list mods (bnd->ms bb) (bnd->ctc bb))))))
           (bnd* (sort bnd* > #:key bnd->ms)))
      (cond
      [(null? bnd*)
       (cond
       [(state1?)
        (next-state!)
        (values '(success S*=>D*) (string-swap* cfg #\2 #\1))]
       [else
        (values '(error no-mods) #f)])]
      [else
       (let ((r
         (for/or ((bnd (in-list bnd*)))
           (define mod-info*
             (sort
               (for/list ((mm (in-list (bnd->blame bnd))))
                 (define idx (mod->idx mm))
                 (define knd (string-ref cfg idx))
                 (list knd idx mm))
               char<?
               #:key car))
           (define key (map car mod-info*))
           (cond
            ;; U-D / 0-1 => 1-1
            ;; U-S / 0-2 => 2-2
            ;; D-S / 1-2 => 1-1
            [(equal? key '(#\0 #\1))
             (list '(success U-D->D-D) (string-update cfg (cadr (car mod-info*)) #\1))]
            [(equal? key '(#\0 #\2))
             (list '(success U-S->S-S) (string-update cfg (cadr (car mod-info*)) #\2))]
            [(equal? key '(#\1 #\2))
             (list '(success D-S->D-D) (string-update cfg (cadr (cadr mod-info*)) #\1))]
            [else
             (raise-arguments-error 'bnd:greedy
               "unknown key / boundary"
               "key" key
               "boundary info" mod-info*
               "cfg" cfg
               "bm" bm-name)])))
           )
           (if r
           (apply values r)
        (values '(error no-actionable) #f)))]))))

(define (blame-whitelist? bm-name mm blm)
  (define (match? . rx*)
    (for*/or ((rx (in-list rx*))
              (bb (in-list blm)))
      (regexp-match? rx bb)))
  (or
    (match? #rx"IDKABOUTHTHISformat\\.rkt")
    (case bm-name
      #;(("kcfa")
       (or
         #;(match? #rx"Closure?" #rx"Closure-benv" #rx"Closure-lam" #rx"Binding?"
           #rx"Binding-var" #rx"Binding-time" #rx"Store?" #rx"Store-call"
           #rx"Store-benv" #rx"Store-store" #rx"Store-time" #rx"Stx?"
           #rx"Stx-label" #rx"exp?" #rx"Ref?" #rx"Ref-var" #rx"Lam?"
           #rx"Lam-formals" #rx"Lam-call" #rx"Call?" #rx"Call-fun" #rx"Call-args")
         #false))
      (else
       #f))))

(define (bnd->blame bb)
  (car bb))

(define (bnd->ms bb)
  (cadr bb))

(define (bnd->ctc bb)
  (caddr bb))

(define (submod-path? str)
  (define mm (regexp-match #rx"^\\([^ ]+ ([^)]+)\\)$" str))
  (and mm (list (car mm) (string-append (cadr mm) ".rkt"))))

(define (interface-for? str)
  (regexp-match #rx"interface for ([^)]+)\\)" str))

(define (srcloc-filename? str)
  (regexp-match #rx"^([^/]+\\.rkt):[0-9]+:[0-9]+" str))

(define (parse-profile-data:json fn)
  (with-input-from-file
    fn
    (lambda ()
      (ignore-line fn #rx"^cpu time")
      (define prf#
        (with-handlers ((exn:fail? (lambda (xx) (printf "exn parsing ~s~n" fn) (raise xx))))
          (read)))
      (for/hash (((k v) (in-hash prf#)))
        (if (eq? k 'nodes)
          (values k (for/list ((nn (in-list v)) (idx (in-naturals)))
                      (hash-set nn 'idx idx)))
          (values k v))))))

(define (parse-profile-data:text fn)
  (with-input-from-file
  fn
  (lambda ()
  (for/list ((ln (in-lines))
  #:when (name+src-line? ln))
  (with-handlers ((exn:fail:contract? (lambda (ex) (raise-arguments-error 'parse-profile
  "error parsing" "file" fn "line" ln "message" (exn-message ex)))))
  (parse-name+src ln))))))

(define (name+src-line? str)
  (regexp-match? #rx"^\\[[0-9]+[]]" (string-trim str)))

(define (parse-name+src str)
  (let* ((str (string-trim str))
  (str* (cdr (regexp-match #rx"^([^ ]+) +([^ ].*)$" str)))
  ;; (_0 (printf "match 0 ~s~n" str*))
  (idx (string->number (cadr (regexp-match #rx"^\\[([0-9]+)[]]" (car str*)))))
  (str* (cdr (regexp-match #rx"^([^ ]+) +([^ ].*)$" (cadr str*))))
  ;; (_1 (printf "match 1 ~s~n" str*))
  (total% (parse-pct (car str*)))
  (str* (cdr (regexp-match #rx"^([^ ]+) +([^ ].*)$" (cadr str*))))
  ;; (_2 (printf "match 2 ~s~n" str*))
  (self% (parse-pct (car str*)))
  (name+src (cadr str*))
  )
  (profile-sample idx total% self% name+src)))

(define (parse-pct str)
  (define mm (regexp-match #rx"\\(([0-9]+\\.[0-9]+)%\\)" str))
  (if mm
  (string->number (cadr mm))
  (raise-argument-error 'parse-pct "parse error" str)))

(define (bndpath-cpu-time fn)
  (with-input-from-file
    fn
    (lambda ()
      (time-string->cpu-time (read-line)))))

(define (parse-bnd-data fn)
  (with-input-from-file
    fn
    (lambda ()
      (ignore-line fn #rx"^cpu time")
      (ignore-line fn #rx"^Running time")
      (ignore-line fn #rx"ms$")
      (ignore-line fn #rx"^ *$")
      (let loop ((l0 (read-line)))
        (define l1 (and (not (eof-object? l0)) (read-line)))
        (define-values [l2 l3]
          (if (or (not l1) (eof-object? l1))
            (values #f #f)
            (let loop ((l2 (read-line)) (l3 (read-line)))
              (if (or (eof-object? l3) (not (string-prefix? l3 " ")))
                (values l2 l3)
                (let-values ([(l2* l3*) (loop l3 (read-line))])
                  (values (string-append l2 " " l2*) l3*))))))
        (if (not l2)
          '()
          (cons
            (with-handlers ((exn:fail:contract?
                              (lambda (ee)
                                (raise-arguments-error 'parse-bnd-data "death by parse error"
                                  "filename" fn
                                  "line 0" l0
                                  "line 1" l1
                                  "line 2" l2
                                  "orig message" (exn-message ee)))))
              (list (bnd:parse-boundary l0) (bnd:parse-ms l1) (bnd:parse-ctc l2)))
            (loop l3)))))))

(define (ignore-filename? str)
  (or (regexp-match? #rx"racket-8" str)
      (regexp-match? #rx"no-negative-party" str)
      (regexp-match? #rx"lib syntax" str)
      (regexp-match? #rx"<collects>" str)
      (regexp-match? #rx"async-channel\\.rkt" str)
      (regexp-match? #rx"dict\\.rkt" str)
      (regexp-match? #rx"boundmap\\.rkt" str)
      (regexp-match? #rx"contract\\.rkt" str)
      (regexp-match? #rx"blame-no-swap" str)
      (regexp-match? #rx"blame-yes-swap" str)
      ))

; /: contract violation
;   expected: number?
;   given: '(("0000000" 8735/8) ("0000001" 5227/2) ("0000002" 23055/8) ("0000010" 4307/4) ("0000020" 8761/8) ("0000011" 10439/4) ("0000012" 23527/8) ("0000021" 10585/4) ("0000022" 2900) ("0000100" 4313/4) ("0000200" 8647/8) ("0000101" 2621) ("0000102" 22867/8) ("0...
;   context...:

(define (ignore-line fn rx)
  (define ln (read-line))
  (cond
   [(eof-object? ln)
    (raise-arguments-error 'ignore-line "unexpected EOF" "file" fn "seeking" rx "port" (current-input-port))]
   [(regexp-match? rx ln)
    (void)]
   [else
    (raise-arguments-error 'ignore-line "line does not match regexp" "line" ln "regexp" rx "file" fn "port" (current-input-port))]))

(define (bnd:parse-boundary str)
  (with-input-from-string str read))

(define (bnd:parse-ms str)
  (define mm (regexp-match #rx"([0-9]+\\.?[0-9]*) ms" (string-trim str)))
  (if mm
    (string->number (cadr mm))
    (raise-argument-error 'bnd:parse-ms "string [N.N ms]" str)))

(define (bnd:parse-ctc str)
  (string-trim str))

(define (module-names bm-name)
  (define bm-dir (benchmark->home-dir bm-name))
  (define u-dir (build-path bm-dir "untyped"))
  (sort
    (map (compose1 path->string file-name-from-path)
      (glob (build-path u-dir "*rkt")))
   string<?))

(define (typed-module? cfg modidx)
  (typed-bit? (string-ref cfg modidx)))

(define (typed-bit? c)
(memq c '(#\1 #\2)))

(define (deep-module? cfg modidx)
  (deep-bit? (string-ref cfg modidx)))

(define (deep-bit? c)
(eq? c #\1))

(define (shallow-module? cfg modidx)
  (shallow-bit? (string-ref cfg modidx)))

(define (shallow-bit? c)
(eq? c #\2))

(define (untyped-module? cfg modidx)
  (untyped-bit? (string-ref cfg modidx)))

(define (untyped-bit? c)
(eq? c #\0))

(define (run-exp bm-name pi find-next-cfg)
  (for/list ((ci (in-list pi))
             (ii (in-naturals)))
    (when (or (< ii 10)
              (zero? (modulo ii 10000)))
      (printf "run-exp: cfg ~s~n" ii))
    (define start-cfg (cfg-id ci))
    (define-values [status end-perf step*] (follow-trail start-cfg pi find-next-cfg))
   ;; (printf "follow trail: ~a => ~a ~a~n" start-cfg status end-perf)
    (row
      start-cfg
      status
      end-perf
      (length step*)
      step*)))

(define (follow-trail start-cfg pi find-next-cfg)
  (let loop ([cfg start-cfg]
             [step* (list start-cfg)]
             ;[prf (cfg-perf ci)]
             )
    (define-values [st cfg+] (find-next-cfg cfg))
    (cond
    [(success-status? st)
     (define step+ (cons cfg+ step*))
     (define pp (pi-perf pi cfg+))
     (if (done-cfg? cfg+ (overhead pp pi))
       (values st pp (reverse step+))
       (loop cfg+ step+))]
    [else
     (values st (pi-perf pi cfg) (reverse step*))])))

(define (plot-full-output run#)
  (define bm-name
    (let* ((name (hash-ref run# key:bm))
           (strat (hash-ref run# key:strategy)))
      (format "~a-~a" name strat)))
  (define profile* (hash-ref run# key:prf))
  (define p2* (hash-ref run# key:prf2))
  (define bnd* (hash-ref run# key:bnd))
  (define pi (hash-ref run# key:pi))
  ;; TODO same y axis for horizontal plots!!!
  (void
    (plot-out bm-name key:prf pi profile*)
    (plot-out bm-name key:prf2 pi p2*)
    (plot-out bm-name key:bnd pi bnd*)))

(define (plot-out bm-name k:mode pi row*)
  (define data-file (format "~a/~a-~a.rktd" out-dir bm-name k:mode))
  (void
    (with-output-to-file data-file #:exists 'replace
      (lambda ()
      (pretty-write row*)))
    (eprintf "saved data: ~s~n" data-file))
  (define n-success (count-success row*))
  (define n-fast (count-fast row* pi))
  (void
    (save-pict+ (format "~a/~a-overhead-~a.png" out-dir bm-name k:mode)
     (histogram-overhead pi row* n-fast bm-name k:mode))
    (save-pict+ (format "~a/~a-steps-~a.png" out-dir bm-name k:mode)
     (histogram-steps row* n-fast bm-name k:mode)))
  (printf "~a ~a quick stats:~n" bm-name k:mode)
  (printf " ~a improved scenarios~n" n-success)
  (printf " ~a scenarios ended under ~ax~n" n-fast overhead-hi)
  (void))

(define (save-pict+ str pp)
  (save-pict str pp)
  (eprintf "save-pict: ~s~n" str)
  (void))

(define (count-fast row* pi)
  (count-perf row* (lambda (tt) (<= (overhead tt pi) overhead-hi))))

(define (histogram-overhead pi row* num-fast bm-name k:mode)
  (parameterize ((plot-x-label "End Overhead vs untyped")
                 (plot-y-label "Trails")
                 (plot-title (format "~a ~a (success: ~a/~a)"
                                     bm-name k:mode
                                     (or num-fast "??") (length row*))))
    (plot-rect
      (for/fold ((acc (hash)))
                 ((r (in-list row*)))
        (hash-update acc (rnd (overhead (row-end-perf r) pi)) add1 (lambda () 0)))
      #:color 3)))

(define (histogram-steps row* num-fast bm-name k:mode)
  (parameterize ((plot-x-label "Steps")
                 (plot-y-label "Trails")
                 (plot-title (format "~a ~a (success: ~a/~a)"
                                     bm-name k:mode
                                     (or num-fast "??") (length row*))))
    (plot-histogram
      (for/fold ((acc (hash)))
                ((r (in-list row*)))
        (hash-update acc (row-n-steps r) add1 (lambda () 0)))
      #:color 2)))

(define (plot-histogram h# #:color [color 1])
  (plot-pict
    (discrete-histogram
    (hash->cat-vals h#)
    #:alpha 0.8
    #:color color)))

(define (plot-rect h# #:color [color 1])
  (define k-max
    (max overhead-hi
      (exact-ceiling
        (for/fold ((acc 0)) ((k (in-hash-keys h#)))
          (max acc (string->number k))))))
  (define delta (/ k-max 100))
  (plot-pict
    (rectangles
      (for/list ((kv (in-list (sort (map (lambda (kv) (cons (string->number (car kv)) (cdr kv))) (hash->list h#)) < #:key car))))
        (define n (car kv))
        (define v (cdr kv))
        (vector (ivl n (+ n delta))
                (ivl 0 v)))
      #:alpha 0.4
      #:color color)
    #:x-min 1
    #:x-max k-max
    ))

(define (count-success row*)
  (for/sum ((r (in-list row*))
            #:when (success-status? (row-end-status r)))
  1))

(define (success-status? x)
  (eq? (car x) 'success))

(define (count-perf row* fn)
  (for/sum ((r (in-list row*))
  #:when (fn (row-end-perf r)))
  1))

(define (hash->cat-vals h#)
  (for/list (((k v) (in-hash h#)))
    (vector k v)))

(define (>> x0 x1)
  (or (> (first x0) (first x1))
      (and (= (first x0) (first x1))
           (> (second x0) (second x1)))))

;; ---

(module+ main
  (require racket/cmdline)
  (define *strategy (box 'greedy))
  (command-line
    #:once-each
    [("-s") str "strategy" (set-box! *strategy (string->symbol str))]
    #:args (bm-name)
    (plot-full-output (run-full-experiment bm-name (unbox *strategy)))
    (print-warnings)
    (void)))
