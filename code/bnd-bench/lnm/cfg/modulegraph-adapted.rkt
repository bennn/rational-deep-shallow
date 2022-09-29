#lang typed/racket/base

(require
  require-typed-check)

(require/typed/check "modulegraph.rkt"
  [#:struct modulegraph (
    [project-name : String]
    [adjlist : (Listof (Listof String))])]
)
;; TODO can use opaque types instead?
(define-type ModuleGraph modulegraph)

(provide ModuleGraph)

(module* summary typed/racket/base
  (require (submod "..") require-typed-check)
  (require/typed/check "modulegraph.rkt"
    [project-name (-> ModuleGraph String)]
    [from-tex (-> Path-String ModuleGraph)]
    [module-names (-> ModuleGraph (Listof String))]
    [path->project-name (-> Path String)])
  (provide (all-from-out (submod "..")) project-name from-tex module-names path->project-name))
