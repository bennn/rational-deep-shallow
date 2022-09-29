  (define foos
    (for/list ((i (in-range 3)))
      (struct foo (f))
      foo))
;; can we exploit this to go across boundaries?!
