#lang info

(define name "xsd-gc")

(define deps '("racket" "threading-lib"))

(define raco-commands 
  '(("xsd-gc"                                 ; command
     xsd-gc/tool                              ; module path
     "remove unused types from an XML schema" ; description
     #f)))                                    ; prominence (#f -> hide)
