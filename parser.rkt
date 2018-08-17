#lang curly-fn racket/base

(require
 data/monad
 data/applicative
 megaparsack
 megaparsack/text
 racket/match
 racket/string
 )

(provide law/p)

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
  (lexeme (one-of/p '(#\º #\o))))

(define hyphen-sign/p
  (lexeme (one-of/p '(#\- #\‒ #\– #\−))))

(define (symbol/p str)
  (lexeme (string/p str)))

(define (symbol-ci/p str)
  (lexeme (string-ci/p str)))

(define int/p
  (lexeme integer/p))

(define int-ordinal/p
  (do
      [n <- int/p]
      (define ordinal-or-dot/p
        (if (> n 9)
            (symbol/p ".")
            ordinal-sign/p))
      ordinal-or-dot/p
      (pure n)))

;; law parser
; livro, subsecao, parte, etc, ver lei complementar 95
(define (make-item kind n text)
  (cons (cons kind n) text))

(define (make-item-parser kind del/p num/p)
  (do
      del/p
      [n <- num/p]
      [t <- paragraph/p]
      (pure (make-item kind n t))))

(define titulo/p
  (make-item-parser 'titulo (symbol-ci/p "TÍTULO") roman-numeral/p))

(define capitulo/p
  (make-item-parser 'capitulo (symbol-ci/p "CAPÍTULO") roman-numeral/p))


(define secao/p
  (make-item-parser 'secao (symbol-ci/p "Seção") roman-numeral/p))

(define artigo/p
  (make-item-parser 'artigo
                    (symbol-ci/p "Art.")
                    int-ordinal/p))

(define (paragrafo/parser)
  (define unico/p
    (do
        (symbol-ci/p "Parágrafo")
        (symbol-ci/p "único")
        (symbol/p ".")
        (pure 0)))
  (define nao-unico/p
    (do
        (symbol-ci/p "§")
        [n <- int-ordinal/p]
        (pure n)))
  (make-item-parser 'paragrafo void/p (or/p nao-unico/p unico/p)))
(define paragrafo/p (paragrafo/parser))

(define inciso/p
  (make-item-parser 'inciso void/p (<* roman-numeral/p
                                       hyphen-sign/p)))
(define alinea/p
  (make-item-parser 'alinea void/p
                    (<* (lexeme (satisfy/p char-alphabetic?))
                        (symbol/p ")"))))

(define item/p
  (make-item-parser 'item void/p
                    int/p))

(define law/p
  (<*
   (many+/p
    (apply or/p (map try/p (list titulo/p capitulo/p secao/p artigo/p
                                 paragrafo/p inciso/p alinea/p item/p))))
   eof/p))
