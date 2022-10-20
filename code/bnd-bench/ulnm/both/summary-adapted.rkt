#lang typed/racket/base

(require
 require-typed-check/unsafe
 "modulegraph-adapted.rkt"
 )

(unsafe-require/typed/check "summary.rkt"
  [#:struct summary (
    [source : Path-String]
    [dataset : (Vectorof (Listof Index))]
    [modulegraph : ModuleGraph])]
)

(define-type Summary summary)

(provide (struct-out summary) Summary)

(module* lnm-plot typed/racket/base
  (require (submod "..") require-typed-check "modulegraph-adapted.rkt")
  (require/typed/check "summary.rkt"
    [summary-source (-> summary Path-String)]
    [summary-dataset (-> summary (Vectorof (Listof Index)))]
    [summary-modulegraph (-> summary ModuleGraph)]
    [from-rktd (->* [String] [#:graph (U Path #f)] Summary)]
    [all-variations (-> Summary (Sequenceof String))]
    [get-num-variations (-> Summary Index)]
    [get-project-name (-> Summary String)]
    [predicate->variations (-> Summary (-> String Boolean) (Sequenceof String))]
    [untyped-mean (-> Summary Real)]
    [variation->mean-runtime (-> Summary String Real)])
  (provide (all-from-out (submod ".."))
    summary-source summary-dataset summary-modulegraph
    from-rktd all-variations get-num-variations get-project-name predicate->variations untyped-mean variation->mean-runtime))

(module* main typed/racket/base
  (require (submod "..") require-typed-check "modulegraph-adapted.rkt")
  (require/typed/check "summary.rkt"
    [summary-source (-> summary Path-String)]
    [summary-dataset (-> summary (Vectorof (Listof Index)))]
    [summary-modulegraph (-> summary ModuleGraph)]
    [from-rktd (->* [String] [#:graph (U Path #f)] Summary)]
    [all-variations (-> Summary (Sequenceof String))]
    [get-num-variations (-> Summary Index)]
    [get-project-name (-> Summary String)]
    [predicate->variations (-> Summary (-> String Boolean) (Sequenceof String))]
    [untyped-mean (-> Summary Real)]
    [variation->mean-runtime (-> Summary String Real)])
  (provide (all-from-out (submod ".."))
    summary-source summary-dataset summary-modulegraph
    from-rktd all-variations get-num-variations get-project-name predicate->variations untyped-mean variation->mean-runtime))
