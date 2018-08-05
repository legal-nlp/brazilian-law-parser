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
(define any-char/p
  (satisfy/p (lambda (_) #t)))

(define (string-ci/p str)
  (if (zero? (string-length str))
      (pure "")
      (label/p str (do (char-ci/p (string-ref str 0))
                       (string-ci/p (substring str 1))
                       (pure str)))))

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
       (pure (list->string p))))

(define ws/p
  (many/p (satisfy/p char-whitespace?)))
  

;; law parser
(define titulo/p undefined)
(define capitulo/p undefined)
(define secao/p undefined)

(define artigo/p
  (do
      (string-ci "Art")
      ws/p
      (char/p #\.)
      ws/p
      [n <- integer/p]
      ws/p
      [t <- paragraph]
      ws/p
      (pure (cons (cons 'artigo n) t))))

(define paragrafo/p undefined)
(define inciso/p undefined)
(define alinea/p undefined)
(define item/p undefined)

(define law/p
  (many/p (or/p titulo/p capitulo/p secao/p artigo/p paragrafo/p inciso/p alinea/p item/p) #:min 1))
