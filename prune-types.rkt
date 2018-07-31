#lang racket

(provide prune-types)

(require "type-util.rkt"
         threading)

#| Keep in mind the xexpr grammar:

xexpr = string
      | (list symbol (list (list symbol string) ...) xexpr ...)
      | (cons symbol (list xexpr ...))
      | symbol
      | valid-char?
      | cdata
      | misc
|#

(define (filter-children predicate xexpr)
  ; Return a copy of the specified xexpr where any of its descendants may be
  ; missing based on the return value of the specified predicate invoked with
  ; the descendant. #t -> keep, #f -> elide.
  (match xexpr
    [(list tag-name (list attributes ...) children ...)
     `(,tag-name ,attributes ,@(filter predicate children))]

    [otherwise otherwise]))

(define (declared-type xexpr)
  ; Return the name of the type if the specified xexpr is a complexType
  ; definition. Otherwise, return #f.
  (match xexpr
    [(xsd-type name _ _) name]
    [_ #f]))

(define (prune-types xexpr types-to-remove)
  (let ([types (~>> types-to-remove (map without-namespace) (apply set))])
    (filter-children 
      (lambda (child) (not (set-member? types (declared-type child))))
      xexpr)))
