#lang racket/base

(require racket/list)

(provide disp-hierarchy
         disp-order
         roman->int
         alpha->int)

; (hash Disp Int)
(define disp-hierarchy
  (for/hasheq ([d '(item alinea inciso paragrafo artigo subsecao
                         secao capitulo titulo norma)]
               [n (in-naturals 1)])
    (values d n)))

(define (disp-order d)
  (hash-ref disp-hierarchy d))

(define (roman->int number)
  (define letter-values
    (hash #\M 1000 #\m 1000 #\D 500 #\d 500 #\C 100 #\c 100 #\L 50 #\l 50 #\X 10 #\x 10 #\V 5 #\v 5 #\I 1 #\i 1))
  (define (get-value letter)
    (hash-ref letter-values letter))
  (define lst (map get-value number))
  (+ (last lst)
     (for/fold ((sum 0))
       ((i (in-list lst)) (i+1 (in-list (rest lst))))
       (+ sum
          (if (> i+1 i)
              (- i)
              i)))))

(define (alpha->int alph)
  (- (char->integer alph) 96))
