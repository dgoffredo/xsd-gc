#lang racket

(provide prune-types)

(require "type-util.rkt"
         threading)

(define (whitespace? datum)
  (and (string? datum) (regexp-match #px"\\s+" datum)))

(define (remove-with-leading-whitespace what lst)
  (match lst
    ; Has what we want to remove, and leading whitespace.
    [(list before ... (? whitespace? _) (== what) after ...)
     (remove-with-leading-whitespace what (append before after))]

    ; Has what we want to remove, without leading whitespace.
    [(list before ... (== what) after ...)
     (remove-with-leading-whitespace what (append before after))]

    ; Doesn't have what we want to remove.
    [otherwise lst]))

#| Keep in mind the xexpr grammar:

xexpr = string
      | (list symbol (list (list symbol string) ...) xexpr ...)
      | (cons symbol (list xexpr ...))
      | symbol
      | valid-char?
      | cdata
      | misc
|#

(define (filter-children keep? xexpr)
  ; Return a copy of the specified xexpr where any of its descendants may be
  ; missing based on the return value of the specified predicate invoked with
  ; the descendant. When a child is elided, any leading whitespace before the
  ; removed element is also removed.
  (let ([placeholder (gensym)]) ; ID to mark children for removal
    (match xexpr
      [(list tag-name (list attributes ...) children ...)
       `(,tag-name 
         ,attributes 
         ; Return the children (recursing on each), but if (keep? child) is
         ; #f for any of them, remove it and any whitespace that precedes it.
         ; A removal happens by first replacing (mapping) the child with the
         ; removal placeholder, and then later removing the placeholder with
         ; any preceding whitespace.
         ,@(~>> children
             (map (lambda (child) (filter-children keep? child)))
             (map (lambda (child) (if (keep? child) child placeholder)))
             (remove-with-leading-whitespace placeholder)))]
  
      [otherwise otherwise])))

(define (declared-type xexpr)
  ; Return the name of the type if the specified xexpr is a type definition.
  ; Otherwise, return #f.
  (match xexpr
    [(xsd-type name _ _) name]
    [_ #f]))

(define (prune-types xexpr types-to-remove)
  (let ([types (~>> types-to-remove (map without-namespace) (apply set))])
    (filter-children 
      (lambda (child) (not (set-member? types (declared-type child))))
      xexpr)))
