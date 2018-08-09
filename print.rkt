#lang racket/base

(require racket/match "parser.rkt")
(provide ->pollen)

(define (print-item i)
  (match i
    [(cons (cons k _) t) (displayln (format "◊~a{~a}" k t))]
    [_ (error "shouldn't happen, ever.")]))

(define (->pollen res)
  (displayln "#lang pollen\n\n")
  (for-each print-item res))
