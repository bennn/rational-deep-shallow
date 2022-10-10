#lang typed/racket/base

(provide Label Tree Node
  (struct-out label)
  (struct-out node)
  (struct-out suffix-tree))

(require require-typed-check/unsafe)

(unsafe-require/typed/check "data.rkt"
  [#:struct label ([datum : (Vectorof (U Char Symbol))]
                   [i : Natural] [j : Natural])
                   #:extra-constructor-name make-label]
  [#:struct node ([up-label : Label]
                  [parent : (U #f Node)]
                  [children : (Listof Node)]
                  [suffix-link : (U #f Node)])
                  #:extra-constructor-name make-node]
  [#:struct suffix-tree ([root : Node])
  #:extra-constructor-name make-suffix-tree])

(define-type Label label)
(define-type Tree suffix-tree)
(define-type Node node)

;; ---

(module* label typed/racket/base
  (require (submod "..") require-typed-check)
  (require/typed/check "data.rkt"
   [label-datum (-> label (Vectorof (U Char Symbol)))]
   [label-i (-> label Natural)]
   [label-j (-> label Natural)]
   [node-up-label (-> Node Label)]
   [node-parent (-> Node (U #f Node))]
   [node-children (-> Node (Listof Node))]
   [node-suffix-link (-> Node (U #f Node))]
   [suffix-tree-root (-> suffix-tree  Node)]
    [make-label (-> (Vectorof (U Char Symbol)) Natural Natural Label)]
    [make-suffix-tree (-> Node Tree)]
    [make-node (-> Label (U #f Node) (Listof Node) (U #f Node) Node)]
    [set-label-i! (-> Label Natural Void)]
    [set-label-j! (-> Label Natural Void)]
    [set-node-children! (-> Node (Listof Node) Void)]
    [set-node-up-label! (-> Node Label Void)]
    [set-node-parent! (-> Node Node Void)]
    [set-node-suffix-link! (-> Node Node Void)])
  (provide (all-from-out (submod ".."))
    label-datum label-i label-j
    node-up-label node-parent node-children node-suffix-link
    suffix-tree-root
    make-label make-suffix-tree make-node set-label-i!  set-label-j!  set-node-children!  set-node-up-label!  set-node-parent!  set-node-suffix-link!))

(module* lcs typed/racket/base
  (require (submod "..") require-typed-check)
  (require/typed/check "data.rkt"
   [label-datum (-> label (Vectorof (U Char Symbol)))]
   [label-i (-> label Natural)]
   [label-j (-> label Natural)]
   [node-up-label (-> Node Label)]
   [node-parent (-> Node (U #f Node))]
   [node-children (-> Node (Listof Node))]
   [node-suffix-link (-> Node (U #f Node))]
   [suffix-tree-root (-> suffix-tree  Node)]
    [make-label (-> (Vectorof (U Char Symbol)) Natural Natural Label)]
    [make-suffix-tree (-> Node Tree)]
    [make-node (-> Label (U #f Node) (Listof Node) (U #f Node) Node)]
    [set-label-i! (-> Label Natural Void)]
    [set-label-j! (-> Label Natural Void)]
    [set-node-children! (-> Node (Listof Node) Void)]
    [set-node-up-label! (-> Node Label Void)]
    [set-node-parent! (-> Node Node Void)]
    [set-node-suffix-link! (-> Node Node Void)])
  (provide (all-from-out (submod ".."))
    label-datum label-i label-j
    node-up-label node-parent node-children node-suffix-link
    suffix-tree-root
    make-label make-suffix-tree make-node set-label-i!  set-label-j!  set-node-children!  set-node-up-label!  set-node-parent!  set-node-suffix-link!))

(module* structs typed/racket/base
  (require (submod "..") require-typed-check)
  (require/typed/check "data.rkt"
   [label-datum (-> label (Vectorof (U Char Symbol)))]
   [label-i (-> label Natural)]
   [label-j (-> label Natural)]
   [node-up-label (-> Node Label)]
   [node-parent (-> Node (U #f Node))]
   [node-children (-> Node (Listof Node))]
   [node-suffix-link (-> Node (U #f Node))]
   [suffix-tree-root (-> suffix-tree  Node)]
    [make-label (-> (Vectorof (U Char Symbol)) Natural Natural Label)]
    [make-suffix-tree (-> Node Tree)]
    [make-node (-> Label (U #f Node) (Listof Node) (U #f Node) Node)]
    [set-label-i! (-> Label Natural Void)]
    [set-label-j! (-> Label Natural Void)]
    [set-node-children! (-> Node (Listof Node) Void)]
    [set-node-up-label! (-> Node Label Void)]
    [set-node-parent! (-> Node Node Void)]
    [set-node-suffix-link! (-> Node Node Void)])
  (provide (all-from-out (submod ".."))
    label-datum label-i label-j
    node-up-label node-parent node-children node-suffix-link
    suffix-tree-root
    make-label make-suffix-tree make-node set-label-i!  set-label-j!  set-node-children!  set-node-up-label!  set-node-parent!  set-node-suffix-link!))

