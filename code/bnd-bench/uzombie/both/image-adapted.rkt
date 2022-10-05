#lang typed/racket/base

(require require-typed-check/unsafe)

(unsafe-require/typed/check "image.rkt"
  (#:struct image ((impl : Any)))
)
(define-type Image image)

(provide
  (struct-out image)
  Image
)

(module* main typed/racket/base
 (require require-typed-check (submod ".."))
 (require/typed/check "image.rkt"
  (image-impl (-> image Any))
  (empty-scene (-> Real Real Image))
  (place-image (-> Image Real Real Image Image))
  (circle (-> Real String String Image)))
 (provide (all-from-out (submod "..")) image-impl empty-scene place-image circle))

(module* zombie typed/racket/base
 (require require-typed-check (submod ".."))
 (require/typed/check "image.rkt"
  (image-impl (-> image Any))
  (empty-scene (-> Real Real Image))
  (place-image (-> Image Real Real Image Image))
  (circle (-> Real String String Image)))
 (provide (all-from-out (submod "..")) image-impl empty-scene place-image circle))
