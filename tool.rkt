#lang racket

; This is the command line tool. See `raco xsd-gc --help`.

(require "options.rkt"
         "xsd-gc.rkt")

(match (parse-options (current-command-line-arguments))
  [(options in-place? anchored-types input-paths)

   (define (from-current-input)
     (xsd-gc (current-input-port)
             (current-output-port)
             anchored-types))

   (if (empty? input-paths)
     (from-current-input)
     (for ([input-path input-paths])
       (cond
         [(equal? input-path "-")
          (from-current-input)]
    
         [in-place?
          (call-with-atomic-output-file
            input-path
            (lambda (out tmp-path)
              (xsd-gc (open-input-file input-path) 
                      out 
                      anchored-types)))]
    
         [else
          (xsd-gc (open-input-file input-path)
                  (current-output-port) 
                  anchored-types)])))])
