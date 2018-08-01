#lang racket

(require "type-util.rkt"
         xml)

(provide get-cuts)

(define whitespace?
  (let ([regex #px"\\s+"])
    (lambda (text)
      (regexp-match regex text))))

(define-match-expander type-to-remove
  ; Define a `match` pattern, `type-to-remove`, that matches a particular type
  ; of element within an XML document. The element must be an XSD type
  ; definition whose name satisfies the specified `remove?` predicate. When
  ; matched, the beginning and one-past-last character offsets will be bound
  ; to the specified `from` and `to`, respectively.
  (syntax-rules ()
    [(type-to-remove remove? from to)
     (...                                        ; treat ellipses literally
       (element                                  ; type definition
         (location _ _ from)                     ; char offset of beginning
         (location _ _ to)                       ; char offset just past end
         (or (? type-tag? _) (? element-tag? _)) ; type or element
         (list-no-order                          ; attributes
           (attribute _ _ 'name (? remove? _))   ; name matches a removed type
           _ ...)                                ; remaining attributes
         _))]))                                  ; children of type definition

(define (get-cuts content types-to-remove)
  ; Return a list of `(list from to)`, each representing a half open range of
  ; unicode characters to be removed from the XML document whose toplevel
  ; element's content is as specified. The cuts remove from the document the
  ; definitions of the specified types and any preceding whitespace. The cuts
  ; are returned in document order.
  (define remove?
    (let ([types-to-remove (list->set types-to-remove)])
      (lambda (name) 
        (set-member? types-to-remove name))))

  (reverse
    (let recur ([content content] [cuts '()])
      (match content
        [(list (pcdata (location _ _ from) _ (? whitespace? _)) ; leading space
               (type-to-remove remove? _ to)                    ; type def
               remaining ...)
  
         (recur remaining (cons (list from to) cuts))]
  
        [(list (type-to-remove remove? from to) ; type definition
               remaining ...)
  
         (recur remaining (cons (list from to) cuts))]
      
        [(list _ remaining ...)                 ; content to keep
  
         (recur remaining cuts)]
  
        ['() cuts]))))                          ; no more content, return cuts
        
