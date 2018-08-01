#lang racket

(provide xsd-gc)

(require "copy-with-cuts.rkt"
         "document-util.rkt"
         "mark-and-sweep.rkt"
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

(define-syntax-rule (debug-let* debug? ([name form] ...) body ...)
  (let* ([name (if debug?
                 (let ([value form])
                   (displayln (~a 'name ": " (~s value)) (current-error-port))
                   value)
                 form)] ...)
    body ...))

(define (types-to-remove xexpr root-types [debug? #f])
  ; Return the names of types in the specified schema that are not visible by
  ; traversing the member trees of each of the specified root types. If the
  ; optionally specified debugging flag is not #f, print intermediate results
  ; to the current error port.
  (debug-let* debug?
              ([graph     (xexpr->digraph xexpr debug?)]
               [to-remove (mark-and-sweep* graph root-types)])
    to-remove))

(define (xsd-gc in-string out-port root-types [debug? #f])
  ; Read an XSD from the specified input port and remove from it all types not
  ; accessbile by traversing the member trees of each of the specified root
  ; types. If root types is #f, then deduce them from the "requestType" and
  ; "responseType" attributes of the schema. Write the resulting XSD to the
  ; specified output port. If the optionally specified debugging flag is
  ; not #f, print intermediate results to the current error port.
  (match (read-xml (open-input-string in-string))
    [(document prolog element extra)
     (debug-let* debug? 
                 ([xexpr      (xml->xexpr element)]
                  [roots      (or root-types (request/response-types xexpr))]
                  [dead-types (types-to-remove xexpr roots debug?)]
                  [cuts       (get-cuts (element-content element) dead-types)])
       (copy-with-cuts cuts (open-input-string in-string) out-port))]))
