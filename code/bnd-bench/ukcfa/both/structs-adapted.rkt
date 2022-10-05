#lang typed/racket/base

(require
  require-typed-check/unsafe
)

(unsafe-require/typed/check "structs.rkt"
  [#:struct Stx ([label : Label])]
  [#:struct (exp Stx) ()]
  [#:struct (Ref exp) ([var : Var])]
  [#:struct (Lam exp) ([formals : (Listof Var)] [call : Exp])]
  [#:struct (Call Stx) ([fun : Exp] [args : (Listof Exp)])]
)

(provide
  (struct-out Stx)
  (struct-out exp)
  (struct-out Ref)
  (struct-out Lam)
  (struct-out Call)
  Exp
  Label
  Var
)

(define-type Exp (U exp Ref Lam Call))
(define-type Label Symbol)
(define-type Var Symbol)

(module* ai typed/racket/base
  (require (submod "..") require-typed-check)
 (require/typed/check "structs.rkt"
  [Stx-label (-> Stx Label)]
  [Ref-var (-> Ref Var)]
  [Lam-formals (-> Lam (Listof Var))]
  [Lam-call (-> Lam Exp)]
  [Call-fun (-> Call Exp)]
  [Call-args (-> Call (Listof Exp))])
 (provide (all-from-out (submod "..")))
 (provide Stx-label Ref-var Lam-formals Lam-call Call-fun Call-args))

(module* benv typed/racket/base
  (require (submod "..") require-typed-check)
 (require/typed/check "structs.rkt"
  [Stx-label (-> Stx Label)]
  [Ref-var (-> Ref Var)]
  [Lam-formals (-> Lam (Listof Var))]
  [Lam-call (-> Lam Exp)]
  [Call-fun (-> Call Exp)]
  [Call-args (-> Call (Listof Exp))])
 (provide (all-from-out (submod "..")))
 (provide Stx-label Ref-var Lam-formals Lam-call Call-fun Call-args))

(module* denotable typed/racket/base
  (require (submod "..") require-typed-check)
 (require/typed/check "structs.rkt"
  [Stx-label (-> Stx Label)]
  [Ref-var (-> Ref Var)]
  [Lam-formals (-> Lam (Listof Var))]
  [Lam-call (-> Lam Exp)]
  [Call-fun (-> Call Exp)]
  [Call-args (-> Call (Listof Exp))])
 (provide (all-from-out (submod "..")))
 (provide Stx-label Ref-var Lam-formals Lam-call Call-fun Call-args))

(module* main typed/racket/base
  (require (submod "..") require-typed-check)
 (require/typed/check "structs.rkt"
  [Stx-label (-> Stx Label)]
  [Ref-var (-> Ref Var)]
  [Lam-formals (-> Lam (Listof Var))]
  [Lam-call (-> Lam Exp)]
  [Call-fun (-> Call Exp)]
  [Call-args (-> Call (Listof Exp))])
 (provide (all-from-out (submod "..")))
 (provide Stx-label Ref-var Lam-formals Lam-call Call-fun Call-args))

(module* time typed/racket/base
  (require (submod "..") require-typed-check)
 (require/typed/check "structs.rkt"
  [Stx-label (-> Stx Label)]
  [Ref-var (-> Ref Var)]
  [Lam-formals (-> Lam (Listof Var))]
  [Lam-call (-> Lam Exp)]
  [Call-fun (-> Call Exp)]
  [Call-args (-> Call (Listof Exp))])
 (provide (all-from-out (submod "..")))
 (provide Stx-label Ref-var Lam-formals Lam-call Call-fun Call-args))

(module* ui typed/racket/base
  (require (submod "..") require-typed-check)
 (require/typed/check "structs.rkt"
  [Stx-label (-> Stx Label)]
  [Ref-var (-> Ref Var)]
  [Lam-formals (-> Lam (Listof Var))]
  [Lam-call (-> Lam Exp)]
  [Call-fun (-> Call Exp)]
  [Call-args (-> Call (Listof Exp))])
 (provide (all-from-out (submod "..")))
 (provide Stx-label Ref-var Lam-formals Lam-call Call-fun Call-args))
