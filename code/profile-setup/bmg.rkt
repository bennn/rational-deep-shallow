#lang racket/base

(require
  racket/file
  racket/path
  racket/system
  file/glob)

;; Boundary manager
;; Run 'boundary-profile.rkt` on all files in worklist

(define home "/users/ben_g/rds-cloudlab")
(define racket (string-append home "/racket-8.6.0.2/bin/racket"))
(define raco (string-append home "/racket-8.6.0.2/bin/raco"))
(define default-out "boundary-profile.txt")
(define main.rkt "main.rkt")
(define boundary-profile (string-append home "/boundary-profile.rkt"))
(define untyped "untyped")
(define typed "typed")
(define shallow "shallow")
(define both "both")

(define boundary-mode 'boundary)
(define profile-mode 'profile)

(define (profile-all bmdir worklist #:mode [pre-mode #f])
  (define mode (or pre-mode boundary-mode))
  (define bmname (or (file-name-from-path bmdir) "unknown"))
  (define out-dir bmname)
  (unless (directory-exists? out-dir)
    (make-directory out-dir))
  (define cfg-dir (build-path bmdir "cfg"))
  (void (clear-compiled! (build-path bmdir "base")))
  (unless (directory-exists? cfg-dir)
    (make-directory cfg-dir))
  ;; ---
  (with-input-from-file
    worklist
    (lambda ()
      (for ((cfgstr (in-lines)))
        (copy-configuration cfgstr bmdir cfg-dir)
  (define rrr
    (let* ((r0 (run-profile cfg-dir mode))
           (_v0 (unless r0
                  (eprintf "ERROR check cfg ~s dir ~s~n" cfgstr cfg-dir)))
           (r1 (or r0 (run-profile cfg-dir mode)))
           (_v1 (unless r1
                  (raise-arguments-error 'die "error, check the directory" "cfg" cfgstr "dir" cfg-dir))))
      r1))
  (move-file (build-path cfg-dir default-out) (build-path out-dir cfgstr))
  (void)))))

(define (clear-compiled! base-dir)
  (for ((dd (in-glob (build-path base-dir "**" "compiled"))))
    (delete-directory/files dd #:must-exist? #f)))

(define (copy-configuration cfgstr bmdir cfg-dir)
  (for ((x (in-glob (build-path cfg-dir "*"))))
    (delete-directory/files x #:must-exist? #f))
  (when (directory-exists? (build-path bmdir both))
    (for ((bth (in-glob (build-path bmdir both "*"))))
      (copy-file bth (build-path cfg-dir (file-name-from-path bth)))))
  (define file*
    (map file-name-from-path (directory-list (build-path bmdir typed))))
  (for ((chr (in-string cfgstr))
  (fn (in-list file*)))
    (copy-file (build-path bmdir (char->dir chr) fn)
         (build-path cfg-dir fn)))
  (void))

(define (char->dir chr)
  (case
    chr
    ((#\0) untyped)
    ((#\1) typed)
    ((#\2) shallow)
    (else (raise-argument-error 'char->dir "(or/c #\0 #\1 #\2)" chr))))

(define (run-profile cfg-dir mode)
  (parameterize ((current-directory cfg-dir))
    (cond
     [(eq? mode boundary-mode)
      (run-racket boundary-profile main.rkt)]
     [(eq? mode profile-mode)
      (with-output-to-file
        default-out
        #:exists 'replace
        (lambda ()
          (run-raco "profile" main.rkt)))]
     [else
      (raise-argument-error 'run-profile "(or/c 'boundary 'profile)" mode)])))

(define (run-racket f0 f1)
  (system (format "~a make -j 1 ~a ~a && ~a ~a ~a"
      raco f0 f1
      racket f0 f1)))

(define (run-raco opt f1)
  (system (format "~a make -j 1 ~a && ~a ~a ~a"
      raco f1
      raco opt f1)))

(define (move-file src tgt)
  (rename-file-or-directory src tgt #true))

;; ---

(module+
  main
  (require racket/cmdline)
  (define current-mode (make-parameter boundary-mode))
  (command-line
    #:once-any
      (("--profile" "-p") "raco profile" (current-mode profile-mode))
      (("--boundary" "-b") "racket boundary-profile.rkt" (current-mode boundary-mode))
    #:args (bmdir worklist)
    (profile-all bmdir worklist #:mode (current-mode))))

