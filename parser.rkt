#lang curly-fn racket/base

(require racket/undefined)

(require
 data/monad
 data/applicative
 megaparsack
 megaparsack/text)

(define (f . <* . g)
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
(define roman-numeral/p
  ((many+/p (apply or/p (map char-ci/p '(#\i #\v #\x #\l #\d #\m)))) . <* . ws/p))

(define any-char/p
  (satisfy/p (lambda (_) #t)))

(define (string-ci/p str)
  (if (non-empty-empty-string? str)
      (label/p str (do (char-ci/p (string-ref str 0))
                       (string-ci/p (substring str 1))
                     (pure str)))
      (pure "")))

(define (paragraph)
  (define par
    (do
        [c <- any-char/p]
        (define next
          (if (char=? c #\newline)
              saw-newline
              par))
        [cs <- next]
        (pure (cons c cs))))
  (define saw-newline
    (do
        [ws <- (many/p (satisfy/p char-whitespace?))]
        (define next
          (if (member #\newline ws)
              (pure null)
              par))
        [cs <- next]
        (pure (append ws cs))))
   (do 
       [p <- par]
       ws/p
       (pure (list->string p))))

(define ws/p
  (many/p (satisfy/p char-whitespace?)))
  
(define ordinal/p
  ((apply or/p (map char/p '(#\º #\o))) . <* . ws))

(define (symbol/p str)
  (do
      [s <- (string/p str)]
      ws/p
      (pure s)))

(define (symbol-ci/p str)
  (do
      [s <- (string-ci/p str)]
      ws/p
      (pure s)))

;; law parser
; livro, subsecao, parte, etc, ver lei complementar 95
(define titulo/p undefined)
(define capitulo/p undefined)
(define secao/p undefined)

(define artigo/p
  (do
      (symbol-ci/p "Art")
      (symbol/p ".")
      [n <- integer/p]
      ws/p
      ordinal-sign/p
      [t <- paragraph]
      (pure (cons (cons 'artigo n) t))))

(define paragrafo/p
  (do
      (symbol-ci/p "þ")
      [n <- integer/p] ; paragrafo unico
      ws/p
      ordinal/p
      [t <- paragraph/p]
      (pure (cons 'paragrafo n) t)))

(define inciso/p
  (do
      [n <- roman-numeral]
      (symbol/p "-")
      [t <- paragraph/p]
      (pure (cons (cons 'inciso n) t))))

(define alinea/p
  (do
      [n <- (satisfy/p char-alphabetic?)]
      ws/p
      (symbol/p ")")
      [t <- paragraph/p]
      (pure (cons (cons 'alinea n) t))))

(define item/p
  (do
      [n <- integer/p]
      ws/p
      [t <- paragraph/p]
      (pure (cons (cons 'item n) t))))

(define law/p
  (many/p (or/p titulo/p capitulo/p secao/p artigo/p paragrafo/p inciso/p alinea/p item/p) #:min 1))
