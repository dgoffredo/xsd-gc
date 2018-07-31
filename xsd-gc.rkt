#lang racket

(provide xsd-gc)

(require "mark-and-sweep.rkt"
         "prune-types.rkt"
         "type-deps.rkt"
         "type-util.rkt"
         graph
         threading
         xml)

(define (request/response? attribute-name)
  (member 
    (~> attribute-name symbol->string without-namespace)
    '("requestType" "responseType")))

#| Keep in mind the xexpr grammar:

xexpr = string
      | (list symbol (list (list symbol string) ...) xexpr ...)
      | (cons symbol (list xexpr ...))
      | symbol
      | valid-char?
      | cdata
      | misc
|#

(define (request/response-types xexpr)
  ; Return a list containing the names of the request type and the response
  ; type in the specified schema. Raise a user error if either is not defined.
  (match xexpr
    [(list schema 
       (list-no-order (list (? request/response? _) the-one)
                      (list (? request/response? _) the-other)
                      the-rest ...)
       children ...)

     (list the-one the-other)]

    [otherwise (raise-user-error
                 (string-join 
                   '("No root types were specified, and the specified schema"
                     "does not name the requestType or the responseType as an"
                     "attribute.")))]))
                        

(define (remove-unused-types xexpr root-types)
  ; Treating the specified xexpr as an XSD, return a copy of the schema having
  ; removed from it the definitions of types that are not visible by
  ; traversing the member trees of each of the specified root types.
  (let ([types-to-remove 
         (~> xexpr xexpr->digraph (mark-and-sweep* root-types))])
    (prune-types xexpr types-to-remove)))

(define (xsd-gc in out [root-types #f])
  ; Read an XSD from the specified input port and remove from it all types not
  ; accessbile by traversing the member trees of each of the optionally
  ; specified root types. If root types is #f, then deduce them from the
  ; "requestType" and "responseType" attributes of the schema. Write the
  ; resulting XSD to the specified output port.
  (match (read-xml in)
    [(document prolog element extra)
     (let* ([xexpr  (xml->xexpr element)]
            [roots  (or root-types (request/response-types xexpr))]
            [result (xexpr->xml (remove-unused-types xexpr roots))])
       (write-xml (document prolog result extra) out))]))
