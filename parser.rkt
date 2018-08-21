#lang curly-fn racket/base

(require
 data/applicative
 data/monad
 megaparsack
 megaparsack/text
 racket/format
 racket/list
 racket/match
 racket/string
 txexpr
 xml
 "brazilian-law.rkt"
 )

;(provide law/p)

(define (<* f g)
  (do
      [x <- f]
      g
    (pure x)))

(define (f . *> . g)
  (do
      f
      g))

;; char and string utils
(define (char=/? c k)
  (not (char=? c k)))

(define (char-not/p c)
  (label/p (format "not '~a'" c)
           (satisfy/p #{char=/? c})))

;; auxiliary parsers
(define any-char/p
  (satisfy/p (lambda (_) #t)))

(define (string-ci/p str)
  (if (non-empty-string? str)
      (label/p str (do (char-ci/p (string-ref str 0))
                       (string-ci/p (substring str 1))
                     (pure str)))
      (pure "")))

(define ws/p
  (many/p (satisfy/p char-whitespace?)))

(define (lexeme p)
  (<* p ws/p))

(define (paragraph/parser)
  (define par
    (do
        [c <- (or/p any-char/p eof/p)]
        (define-values (c- next)
          (match c
            [#\newline (values c saw-newline)]
            [(? void?) (values #\newline (pure null))]
            [_ (values c par)]))
        [cs <- next]
        (pure (cons c- cs))))
  (define saw-newline
    (do
        [ws <- ws/p]
        (define next
          (if (member #\newline ws)
              (pure null)
              par))
        [cs <- next]
        (pure (cons #\newline cs))))
   (do 
       [p <- par]
       ws/p
       (pure (list->string p))))
(define paragraph/p (paragraph/parser))

(define roman-numeral/p
  (lexeme (many+/p (one-of/p '(#\i #\v #\x #\l #\d #\m) char-ci=?))))
  
(define ordinal-sign/p
  (lexeme (one-of/p '(#\º #\o #\°))))

(define hyphen-like-characters
  '(#\- #\‒ #\– #\−))

(define hyphen-sign/p
  (lexeme (one-of/p hyphen-like-characters)))

(define (symbol/p str)
  (lexeme (string/p str)))

(define (symbol-ci/p str)
  (lexeme (string-ci/p str)))

(define int/p
  (lexeme integer/p))

(define (ordinal-or-period/p n)
  (if (> n 9)
      (symbol/p ".")
      ordinal-sign/p))

(define lower-alpha-char/p
  (lexeme
   (satisfy/p (lambda (c) (and (char-alphabetic? c)
                               (char-lower-case? c))))))

(define addend/p
  (many/p (try/p
           (*> (one-of/p hyphen-like-characters)
               (lexeme (satisfy/p char-alphabetic?)))) #:max 1))

(define (annotate-result/p sym p)
  ; allows case matching in dispositivo/p
  (do
      [r <- p]
      (pure (cons sym r))))

;; law parser
; livro, subsecao, parte, etc, ver lei complementar 95
(define (make-item kind n addendum elems)
   (txexpr kind `((number ,(~a n))) elems))

(define (make-item-parser kind num/p
                          [post-num/p (lambda (_) void/p)])
  (do
      [n <- num/p]
      [a <- addend/p]
      [pn <- (post-num/p n)]
      [t <- paragraph/p]
      (define children/p
        (if (eq? kind 'item)
            (pure null)
            (many/p (dispositivo</p kind))))
      [cs <- children/p]
      (pure (make-item kind n a (cons t cs)))))

(define titulo/p
  (make-item-parser 'titulo roman-numeral/p))

(define capitulo/p
  (make-item-parser 'capitulo roman-numeral/p))


(define secao/p
  (make-item-parser 'secao roman-numeral/p))

(define subsecao/p
  (make-item-parser 'subsecao roman-numeral/p))

(define artigo/p
  (make-item-parser 'artigo
                    int/p
                    ordinal-or-period/p))
(define unico/p
  (make-item-parser 'paragrafo
                    (pure 0)
                    (lambda (_) (symbol/p "."))))

(define nao-unico/p
  (make-item-parser 'paragrafo
                    int/p
                    ordinal-or-period/p))

(define (inciso/p r)
  (make-item-parser 'inciso
                    (pure r)
                    (lambda (_) hyphen-sign/p)))
(define (alinea/p l)
  (make-item-parser 'alinea
                    (pure l)
                    (lambda (_) (symbol/p ")"))))

(define (item/p n)
  (make-item-parser 'item
                    (pure n)
                    (lambda (_) hyphen-sign/p)))

(define disp-prefix-parsers
  (list (*> (symbol-ci/p "TÍTULO") (pure '(titulo)))
        (*> (symbol-ci/p "CAPÍTULO") (pure '(capitulo)))
        (*> (try/p (symbol-ci/p "Seção")) (pure '(secao)))
        (*> (symbol-ci/p "Subseção") (pure '(subsecao)))
        (*> (symbol-ci/p "Art.") (pure '(artigo)))
        (or/p (*> (symbol/p "§") (pure '(paragrafo)))
              (*> (*> (symbol-ci/p "Parágrafo") (symbol-ci/p "único"))
                  (pure '(paragrafo-unico))))
        (annotate-result/p 'inciso roman-numeral/p)
        (annotate-result/p 'alinea lower-alpha-char/p)
        (annotate-result/p 'item int/p)))

(define (dispositivo</p kind)
  (do
      [w <- (apply or/p (drop disp-prefix-parsers (- (hash-count disp-hierarchy)
                                                     -1
                                                     (disp-order kind))))]
      (define next/p
        (case (car w)
          [(titulo) titulo/p]
          [(capitulo) capitulo/p]
          [(secao) secao/p]
          [(subsecao) subsecao/p]
          [(artigo) artigo/p]
          [(paragrafo) nao-unico/p]
          [(paragrafo-unico) unico/p]
          [(inciso) (inciso/p (cdr w))]
          [(alinea) (alinea/p (cdr w))]
          [(item) (item/p (cdr w))]))
    next/p))

(define law/p
  (*> ws/p
      (<* (many+/p (dispositivo</p 'titulo)) eof/p)))
