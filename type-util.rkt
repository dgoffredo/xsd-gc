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

(define-match-expander xsd-type-node
  (syntax-rules ()
    [(xsd-type-node matching-tag? name-id other-attrs-id children-id)

     (... ; treat "..." literally in the following template
       (or
         ; Either it's just the complex/simple type tag itself,
         (list (? matching-tag? _) 
               (list-no-order `(name ,name-id) other-attrs-id ...)
           children-id ...)

         ; Or it's the complex/simple type tag as a child of an element tag.
         (list (? element-tag? _)
               (list-no-order `(name ,name-id) _ ...)
            _ ...         ; children preceding the type (annotation?)
            ; the type
            (list (? matching-tag? _) 
                  other-attrs-id
              children-id ...)
              _ ...)))])) ; children succeeding the type (comments?)

(define-match-expander xsd-complex-type
  (syntax-rules ()
    [(xsd-complex-type                name-id other-attrs-id children-id)
     (xsd-type-node complex-type-tag? name-id other-attrs-id children-id)]))

(define-match-expander xsd-type
  (syntax-rules ()
    [(xsd-type                name-id other-attrs-id children-id)
     (xsd-type-node type-tag? name-id other-attrs-id children-id)]))
