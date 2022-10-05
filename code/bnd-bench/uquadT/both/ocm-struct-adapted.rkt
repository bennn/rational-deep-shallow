#lang typed/racket/base

(provide
  Index-Type
  Entry-Type
  Value-Type
  No-Value-Type
  Finished-Value-Type
  Matrix-Proc-Type
  Entry->Value-Type
  OCM-Type
  ;; ---
  (struct-out $ocm)
)

;; -----------------------------------------------------------------------------

(require
  require-typed-check/unsafe
)

(unsafe-require/typed/check "ocm-struct.rkt"
  [#:struct $ocm
    ([min-entrys : (Vectorof Entry-Type)]
     [min-row-indices : (Vectorof (U Index-Type No-Value-Type))]
     [finished : Finished-Value-Type]
     [matrix-proc : Matrix-Proc-Type]
     [entry->value : Entry->Value-Type]
     [base : Index-Type]
     [tentative : Index-Type])])

;; =============================================================================

(define-type Index-Type Nonnegative-Integer)
(define-type Entry-Type Any)
(define-type Value-Type Float)
(define-type No-Value-Type Symbol)
(define-type Finished-Value-Type Index-Type)
(define-type Matrix-Proc-Type (Index-Type Index-Type -> Entry-Type))
(define-type Entry->Value-Type (Entry-Type -> Value-Type))

(define-type OCM-Type $ocm)


(module* ocm typed/racket/base
  (require (submod "..") require-typed-check)
  (require/typed/check "ocm-struct.rkt"
    [set-$ocm-tentative! (-> $ocm Index-Type Void)]
    [set-$ocm-min-entrys! (-> $ocm (Vectorof Entry-Type) Void)]
    [set-$ocm-min-row-indices! (-> $ocm (Vectorof (U Index-Type No-Value-Type)) Void)]
    [set-$ocm-finished! (-> $ocm Finished-Value-Type Void)]
    [set-$ocm-base! (-> $ocm Index-Type Void)]
    [$ocm-min-entrys (-> $ocm (Vectorof Entry-Type))]
    [$ocm-min-row-indices (-> $ocm (Vectorof (U Index-Type No-Value-Type)))]
    [$ocm-finished (-> $ocm Finished-Value-Type)]
    [$ocm-matrix-proc (-> $ocm Matrix-Proc-Type)]
    [$ocm-entry->value (-> $ocm Entry->Value-Type)]
    [$ocm-base (-> $ocm Index-Type)]
    [$ocm-tentative (-> $ocm Index-Type)])
  (provide (all-from-out (submod ".."))
    set-$ocm-tentative!  set-$ocm-min-entrys!  set-$ocm-min-row-indices!
    set-$ocm-finished!  set-$ocm-base!  set-$ocm-tentative!
    set-$ocm-min-entrys!  set-$ocm-min-row-indices!  set-$ocm-finished!
    set-$ocm-base!  $ocm-min-entrys $ocm-min-row-indices $ocm-finished
    $ocm-matrix-proc $ocm-entry->value $ocm-base $ocm-tentative))

(module* wrap typed/racket/base
  (require (submod "..") require-typed-check)
  (require/typed/check "ocm-struct.rkt"
    [set-$ocm-tentative! (-> $ocm Index-Type Void)]
    [set-$ocm-min-entrys! (-> $ocm (Vectorof Entry-Type) Void)]
    [set-$ocm-min-row-indices! (-> $ocm (Vectorof (U Index-Type No-Value-Type)) Void)]
    [set-$ocm-finished! (-> $ocm Finished-Value-Type Void)]
    [set-$ocm-base! (-> $ocm Index-Type Void)]
    [$ocm-min-entrys (-> $ocm (Vectorof Entry-Type))]
    [$ocm-min-row-indices (-> $ocm (Vectorof (U Index-Type No-Value-Type)))]
    [$ocm-finished (-> $ocm Finished-Value-Type)]
    [$ocm-matrix-proc (-> $ocm Matrix-Proc-Type)]
    [$ocm-entry->value (-> $ocm Entry->Value-Type)]
    [$ocm-base (-> $ocm Index-Type)]
    [$ocm-tentative (-> $ocm Index-Type)])
  (provide (all-from-out (submod ".."))
    set-$ocm-tentative!  set-$ocm-min-entrys!  set-$ocm-min-row-indices!
    set-$ocm-finished!  set-$ocm-base!  set-$ocm-tentative!
    set-$ocm-min-entrys!  set-$ocm-min-row-indices!  set-$ocm-finished!
    set-$ocm-base!  $ocm-min-entrys $ocm-min-row-indices $ocm-finished
    $ocm-matrix-proc $ocm-entry->value $ocm-base $ocm-tentative))

