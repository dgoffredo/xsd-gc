#lang racket

(provide mark-and-sweep)

(require graph)

(define (mark-and-sweep digraph vertex)
  ; Traverse the specified directed graph starting at the specified vertex,
  ; marking verticies as they are visited. Return the list of vertices not
  ; visited.
  (define-vertex-property digraph marked? #:init #f)

  (let loop ([vertex vertex])
    (marked?-set! vertex #t)
    (for ([neighbor (in-neighbors digraph vertex)])
      (when (not (marked? neighbor))
        (loop neighbor))))

  (for/list ([vertex (in-vertices digraph)] #:when (not (marked? vertex)))
    vertex))

(module+ test
  (require rackunit)

  (define g (directed-graph '((root a) (a b) (b c) (x y) (y c))))
  (check-equal? (mark-and-sweep g 'root) '(x y)))
