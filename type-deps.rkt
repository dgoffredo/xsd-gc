#lang racket

(provide xexpr->digraph
         xexpr->edges)

(require "type-util.rkt"
         racket/generator
         graph
         xml)

(define-syntax-rule (generate-list body0 body ...)
  (for/list ([yielded-value (in-generator body0 body ...)])
    yielded-value))

#| Keep in mind the xexpr grammar:

xexpr = string
      | (list symbol (list (list symbol string) ...) xexpr ...)
      | (cons symbol (list xexpr ...))
      | symbol
      | valid-char?
      | cdata
      | misc
|#

(define (xexpr->vertices schema)
  ; Return a list of strings representing vertices in the type dependency
  ; graph of the specified schema, where each vertex is a type name.
  (generate-list
    (let loop ([xexpr schema])
      (match xexpr
        ; new type, possibly with children
        [(xsd-type name _ children)
  
         (yield name)
         (for ([child children])
           (loop child))]
  
        ; other node, possibly with children
        [(list _ (list (list _ _) ...)
           children ...)
  
         (for ([child children])
           (loop child))]

        ; scalar
        [else (void)]))))

(define (xexpr->edges schema)
  ; Return a list of two-element lists representing directed edges in the type
  ; dependency graph of the specified schema, where an edge '(A B) denotes that
  ; 'B is a member of 'A.
  (generate-list
    (let loop ([current-type #f] 
               [xexpr schema])
      (match xexpr
        ; new type
        [(xsd-complex-type name _ children)

         (for ([child children])
           (loop name child))]
  
        ; type element
        [(list (? element-tag? _)
           (list-no-order (list (or 'type 'ref) element-type) _ ...)
           children ...)
  
          (yield (list current-type (without-namespace element-type)))]
  
        ; other node
        [(list _ (list (list _ _) ...)
           children ...)
  
         (for ([child children])
           (loop current-type child))]

        ; scalar
        [else (void)]))))

(define (xexpr->digraph schema)
  ; Return a directed-graph where a directed edge from A to B indicates that
  ; B is a member of A.
  (let ([graph (directed-graph (xexpr->edges schema))])
    ; Explicitly add vertices (mostly redundantly) to incorporate any isolated
    ; vertices.
    (for ([vertex (xexpr->vertices schema)])
      (add-vertex! graph vertex))

    graph))
