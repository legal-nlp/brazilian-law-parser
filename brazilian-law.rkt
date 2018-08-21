#lang racket/base

(require racket/list)

(provide disp-hierarchy
         disp-order
         roman->int
         alpha->int)

; (hash Disp Int)
(define disp-hierarchy
  (for/hasheq ([d '(item alinea inciso paragrafo artigo subsecao secao capitulo titulo)]
               [n (in-naturals 1)])
    (values d n)))

(define (disp-order d)
  (hash-ref disp-hierarchy d))

(define (roman->int number)
  (define letter-values
    (map cons '(#\M #\D #\C #\L #\X #\V #\I) '(1000 500 100 50 10 5 1)))
  (define (get-value letter)
    (cdr (assq letter letter-values)))
  (define lst (map get-value (string->list number)))
  (+ (last lst)
     (for/fold ((sum 0))
       ((i (in-list lst)) (i+1 (in-list (cdr lst))))
       (+ sum
          (if (> i+1 i)
              (- i)
              i)))))

(define (alpha->int alph)
  (- (char->integer alph) 96))
