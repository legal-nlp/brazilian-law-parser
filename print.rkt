#lang racket/base

(require racket/match "parser.rkt")
(provide ->pollen)

(define (print-item i)
  (match i
    [(cons (cons k _) t) (displayln (format "◊~a{~a}" k t))]
    [_ (error "broken.")]))

(define (->pollen res)
  (displayln "#lang pollen\n\n")
  (for-each print-item res))
