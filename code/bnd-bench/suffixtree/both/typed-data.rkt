#lang typed/racket/base

(provide Label Tree Node make-label make-suffix-tree make-node
  (struct-out label)
  (struct-out node)
  (struct-out suffix-tree))

(require require-typed-check)

(require/typed/check "data.rkt"
  [#:struct label ([datum : (Vectorof (U Char Symbol))]
                   [i : Natural] [j : Natural])]
  [#:struct node ([up-label : Label]
                  [parent : (U #f Node)]
                  [children : (Listof Node)]
                  [suffix-link : (U #f Node)])]
    [make-label (-> (Vectorof (U Char Symbol)) Natural Natural Label)]
    [make-suffix-tree (-> Node Tree)]
    [make-node (-> Label (U #f Node) (Listof Node) (U #f Node) Node)]
  [#:struct suffix-tree ([root : Node])])

(define-type Label label)
(define-type Tree suffix-tree)
(define-type Node node)

;; ---

(module* label typed/racket/base
  (require (submod "..") require-typed-check)
  (require/typed/check "data.rkt"
    [set-label-i! (-> Label Natural Void)]
    [set-label-j! (-> Label Natural Void)]
    [set-node-children! (-> Node (Listof Node) Void)]
    [set-node-up-label! (-> Node Label Void)]
    [set-node-parent! (-> Node Node Void)]
    [set-node-suffix-link! (-> Node Node Void)])
  (provide (all-from-out (submod "..")) make-label make-suffix-tree make-node set-label-i!  set-label-j!  set-node-children!  set-node-up-label!  set-node-parent!  set-node-suffix-link!))

(module* lcs typed/racket/base
  (require (submod "..") require-typed-check)
  (require/typed/check "data.rkt"
    [make-label (-> (Vectorof (U Char Symbol)) Natural Natural Label)]
    [set-label-i! (-> Label Natural Void)]
    [set-label-j! (-> Label Natural Void)]
    [make-suffix-tree (-> Node Tree)]
    [make-node (-> Label (U #f Node) (Listof Node) (U #f Node) Node)]
    [set-node-children! (-> Node (Listof Node) Void)]
    [set-node-up-label! (-> Node Label Void)]
    [set-node-parent! (-> Node Node Void)]
    [set-node-suffix-link! (-> Node Node Void)])
  (provide (all-from-out (submod "..")) make-label make-suffix-tree make-node set-label-i!  set-label-j!  set-node-children!  set-node-up-label!  set-node-parent!  set-node-suffix-link!))

(module* structs typed/racket/base
  (require (submod "..") require-typed-check)
  (require/typed/check "data.rkt"
    [make-label (-> (Vectorof (U Char Symbol)) Natural Natural Label)]
    [set-label-i! (-> Label Natural Void)]
    [set-label-j! (-> Label Natural Void)]
    [make-suffix-tree (-> Node Tree)]
    [make-node (-> Label (U #f Node) (Listof Node) (U #f Node) Node)]
    [set-node-children! (-> Node (Listof Node) Void)]
    [set-node-up-label! (-> Node Label Void)]
    [set-node-parent! (-> Node Node Void)]
    [set-node-suffix-link! (-> Node Node Void)])
  (provide (all-from-out (submod "..")) make-label make-suffix-tree make-node set-label-i!  set-label-j!  set-node-children!  set-node-up-label!  set-node-parent!  set-node-suffix-link!))

