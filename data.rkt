#lang racket/base

(require
 racket/list
 racket/string
 )

(provide
 alpha->integer
 dispositivo->order
 dispositivo-hierarchy
 dispositivo>=?
 integer->alpha
 integer->roman
 roman->integer
 )

; (hash Disp Int)
(define dispositivo-hierarchy
  (for/hasheq ([d '(item alinea inciso paragrafo artigo subsecao
                         secao capitulo titulo livro parte norma)]
               [n (in-naturals 1)])
    (values d n)))

(define (dispositivo->order d)
  (hash-ref dispositivo-hierarchy d))

(define (dispositivo>=? d d-)
  (>= (dispositivo->order d) (dispositivo->order d-)))

(define (roman->integer int)
  (define letter-values
    (hash #\M 1000 #\m 1000 #\D 500 #\d 500 #\C 100 #\c 100 #\L 50 #\l 50 #\X 10 #\x 10 #\V 5 #\v 5 #\I 1 #\i 1))
  (define (get-value letter)
    (hash-ref letter-values letter))
  (define lst (map get-value int))
  (+ (last lst)
     (for/fold ((sum 0))
       ((i (in-list lst)) (i+1 (in-list (rest lst))))
       (+ sum
          (if (> i+1 i)
              (- i)
              i)))))

(define (integer->roman int)
  (define (number->list n)
    (for/fold ([result null])
              ([decimal '(1000 900 500 400 100 90 50 40 10 9  5 4  1)]
               [roman   '(M    CM  D   CD  C   XC L  XL X  IX V IV I)])
      #:break (= n 0)
      (let-values ([(q r) (quotient/remainder n decimal)])
        (set! n r)
        (append result (make-list q roman)))))
  (string-join (map symbol->string (number->list int)) ""))

(define (alpha->integer alph)
  (- (char->integer alph) 96))

(define (integer->alpha int)
 (integer->char (+ (string->number int) 96)))  
