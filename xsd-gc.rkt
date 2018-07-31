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

(define-syntax-rule (echo debug? form)
  (if debug?
    (let ([value form])
      (displayln (~a 'form ": " value) (current-error-port))
      value)
    form))

(define (remove-unused-types xexpr root-types [debug? #f])
  ; Treating the specified xexpr as an XSD, return a copy of the schema having
  ; removed from it the definitions of types that are not visible by
  ; traversing the member trees of each of the specified root types. If the
  ; optionally specified debugging flag is not #f, print intermediate results
  ; to the current error port.
    (let* ([graph           (echo debug? (xexpr->digraph xexpr))]
           [types-to-remove (echo debug? (mark-and-sweep* graph root-types))])
      (when debug?
        (displayln (~a "graph edges: " (~s (get-edges graph))))
        (displayln (~a "graph vertices: " (~s (get-vertices graph)))))

      (prune-types xexpr types-to-remove)))

(define (xsd-gc in out root-types [debug? #f])
  ; Read an XSD from the specified input port and remove from it all types not
  ; accessbile by traversing the member trees of each of the specified root
  ; types. If root types is #f, then deduce them from the "requestType" and
  ; "responseType" attributes of the schema. Write the resulting XSD to the
  ; specified output port. If the optionally specified debugging flag is
  ; not #f, print intermediate results to the current error port.
  (match (read-xml in)
    [(document prolog element extra)
     (let* ([xexpr  (xml->xexpr element)]
            [roots  (or root-types (request/response-types xexpr))]
            [result (xexpr->xml (remove-unused-types xexpr roots debug?))])
       (when debug?
         #;(displayln (~a "parsed xexpr: " (xexpr->string xexpr))
           (current-error-port))
         (displayln (~a "root types: " (~s roots))
           (current-error-port)))

       (write-xml (document prolog result extra) out))]))
