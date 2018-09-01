#lang racket/base

(require
 racket/list
 racket/match
 racket/string
 )

(provide ->pollen)

(define (print-elem i)
  (match i
    [(list* name attrs elems)
     (let-values ([(elem-texts children) (splitf-at elems string?)])
       (displayln (format "◊~a{~a}\n" name (string-trim
                                          (string-join elem-texts)
                                          #:left? #f)))
       (for-each print-elem children))]
    [_ (error "broken.")]))

(define (->pollen res)
  (displayln "#lang pollen\n\n")
  (for-each print-elem res))
