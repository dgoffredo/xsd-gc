#lang racket

(provide parse-options
         (struct-out options))

(struct options (in-place?      ; boolean
                 anchored-types ; list of string
                 input-paths)   ; list of path string
        #:transparent)

(define (parse-options argv)
  (define in-place?      (make-parameter #f))
  (define anchored-types (make-parameter #f))
  (define input-paths
    (command-line
      #:program "xsd-gc"
      #:argv argv
      #:once-each
      [("-i" "--in-place") "Modify the input file in place"
                           (in-place? #t)]
      #:multi
      [("-a" "--anchor") TYPE
                         "Keep type and its decendants"
                         (anchored-types 
                           (cons TYPE (or (anchored-types) '())))]
      #:args input-paths
      input-paths))

  (options 
    (in-place?)
    (anchored-types)
    input-paths))
