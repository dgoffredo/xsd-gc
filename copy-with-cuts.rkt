#lang racket

(provide copy-with-cuts)

(define (copy-port* in out amount [block-size (expt 2 16)])
  ; Copy up to the specified number of Unicode characters from the specified
  ; input port to the specified output port, keeping in memory a string no
  ; larger than the optionally specified size. Return the number of characters
  ; copied.
  (define buffer (make-string (min amount block-size)))

  (let recur ([total-read 0])
    (if (= total-read amount)
      total-read
      (let* ([max-read (min (- amount total-read) (string-length buffer))]
             [num-read (read-string! buffer in 0 max-read)])
        (cond 
          [(eof-object? num-read) total-read]
          [else
           (write-string buffer out 0 num-read)
           (recur (+ total-read num-read))])))))

(define (copy-with-cuts cuts in out)
  ; Copy from the specified input port to the specified output port, eliding
  ; intervals denoted by the specified list of cuts, where each cut is a
  ; `(list from to)`, where `from` is the offset to the first character to
  ; remove and `to` is one plus the offset of the last character to remove.

  (define (copy [amount #f])
    (if amount
      (copy-port* in out amount)
      (copy-port in out)))

  (define (ignore amount)
    (copy-port* in (open-output-nowhere) amount))

  (let recur ([cuts cuts] [offset 0])
    (match cuts
      ['() (copy)]                    ; No cuts remain, so copy the rest.
      [(list (list from to) tail ...) ; Peel off a cut and process it, recur...
       (copy (- from offset))
       (ignore (- to from))
       (recur tail to)])))
