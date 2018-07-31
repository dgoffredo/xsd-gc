#lang racket

(provide mark-and-sweep
         mark-and-sweep*)

(require graph)

(define (mark-and-sweep digraph root-vertex)
  ; Traverse the specified directed graph starting at the specified vertex,
  ; marking verticies as they are visited. Return the list of vertices in the
  ; digraph that were not visited.
  (define-vertex-property digraph marked? #:init #f)

  (let loop ([vertex root-vertex])
    (marked?-set! vertex #t)
    (for ([neighbor (in-neighbors digraph vertex)])
      (when (not (marked? neighbor))
        (loop neighbor))))

  (for/list ([vertex (in-vertices digraph)] #:when (not (marked? vertex)))
    vertex))

(define (mark-and-sweep* digraph root-vertices)
  ; Traverse the specified directed graph starting at an inserted vertex with
  ; an edge leading to each of the specified vertices, marking verticies as
  ; they are visited. Return the list of vertices in the digraph that were not
  ; visited.
  (let* ([root    (gensym)]
         [digraph (graph-copy digraph)])

    (for ([subroot root-vertices])
      (add-directed-edge! digraph root subroot))

    (mark-and-sweep digraph root)))

(module+ test
  (require rackunit)

  (define g (directed-graph '((root a) (a b) (b c) (x y) (y c))))
  (check-equal? (mark-and-sweep g 'root) '(x y)))
