#lang racket

(provide prune-digraph)

(require graph
         "mark-and-sweep.rkt")

(define (prune-digraph digraph to-anchor to-remove)
  ; Return the list of vertices that are no longer reachable from the specified
  ; anchored vertices after the specified edges have been removed.
  (let ([digraph (graph-copy digraph)]
        [root    (gensym)])
    (for ([edge to-remove])
      (match edge
        [(list from to) 
         (remove-directed-edge! digraph from to)]))

    (for ([vertex to-anchor])
      (add-directed-edge! digraph root vertex))

    (mark-and-sweep digraph root)))

(module+ test
  (require rackunit)

  (define digraph (directed-graph '([a b] [a c] [a d]
                                    [b x]
                                    [c y]
                                    [d y]
                                    [y z] [z y]
                                    [x y] [x z]
                                    [i j] [i k]
                                    [j y]
                                    [k z] [k l] [k m]
                                    [l n] [m n])))

  (let ([removed-edges     '([a b])]
        [anchored-vertices '(a m)])
    (check-equal? 
      (apply set (prune-digraph digraph anchored-vertices removed-edges))
      (apply set '(b x i j k l)))))
