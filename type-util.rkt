#lang racket

(provide xsd-type
         xsd-complex-type
         without-namespace 
         complex-type-tag? 
         element-tag?
         simple-type-tag?
         type-tag?)

(require threading)

(define (without-namespace str)
  (match (string-split str ":")
    [(list namespace name) name]
    [(list           name) name]))

(define-syntax-rule (define-tag-predicate predicate-name tag-name)
  (define (predicate-name sym)
    (and (symbol? sym)
        (~> sym symbol->string without-namespace (equal? tag-name)))))

(define-tag-predicate simple-type-tag?  "simpleType")
(define-tag-predicate complex-type-tag? "complexType")
(define-tag-predicate element-tag?      "element")

(define (type-tag? sym)
  (or (complex-type-tag? sym) (simple-type-tag? sym)))

(define-match-expander xsd-complex-type
  (syntax-rules ()
    [(xsd-complex-type name-id other-attrs-id children-id)
     (... ; treat "..." literally in the following template
       (list (? complex-type-tag? _) 
             (list-no-order `(name ,name-id) other-attrs-id ...)
         children-id ...))]))

(define-match-expander xsd-type
  (syntax-rules ()
    [(xsd-type name-id other-attrs-id children-id)
     (... ; treat "..." literally in the following template
       (list (? type-tag? _) 
             (list-no-order `(name ,name-id) other-attrs-id ...)
         children-id ...))]))
