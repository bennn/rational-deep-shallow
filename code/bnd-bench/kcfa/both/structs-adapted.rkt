#lang typed/racket/base

(require
  require-typed-check
)

(require/typed/check "structs.rkt"
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

;; =============================================================================

(define-type Exp (U exp Ref Lam Call))
(define-type Label Symbol)
(define-type Var Symbol)

(module* ai typed/racket/base (require (submod "..")) (provide (all-from-out (submod ".."))))
(module* benv typed/racket/base (require (submod "..")) (provide (all-from-out (submod ".."))))
(module* denotable typed/racket/base (require (submod "..")) (provide (all-from-out (submod ".."))))
(module* main typed/racket/base (require (submod "..")) (provide (all-from-out (submod ".."))))
(module* time typed/racket/base (require (submod "..")) (provide (all-from-out (submod ".."))))
(module* ui typed/racket/base (require (submod "..")) (provide (all-from-out (submod ".."))))
