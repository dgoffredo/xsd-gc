#lang racket

(provide xexpr->digraph)

(require racket/generator
         threading
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

(define (without-namespace str)
  (match (string-split str ":")
    [(list namespace name) name]
    [(list           name) name]))

(define (complex-type-tag? sym)
  (and (symbol? sym)
       (~> sym symbol->string without-namespace (equal? "complexType"))))

(define (element-tag? sym)
  (and (symbol? sym)
       (~> sym symbol->string without-namespace (equal? "element"))))

(define (xexpr->edges schema)
  ; Return a list of two-element list representing directed edges in the type
  ; dependency graph of the specified schema, where '(A B) means that 'B is a
  ; member of 'A.
  (generate-list
    (let loop ([current-type #f] 
               [xexpr schema])
      #;(displayln 
        (format 
          "In loop with current-type ~a and xexpr ~e" current-type xexpr))
      (match xexpr
        ; new type
        [(list (? complex-type-tag? _) (list-no-order `(name ,name) _ ...)
           children ...)
  
         (for ([child children])
           (loop name child))]
  
        ; type element
        [(list (? element-tag? _) (list-no-order `(type ,element-type) _ ...)
           children ...)
  
          (yield (list current-type element-type))]
  
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
  (directed-graph (xexpr->edges schema)))
