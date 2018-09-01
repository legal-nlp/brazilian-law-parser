#lang info

(define collection "brazilian-law")

(define deps '(["base" #:version "6.5"]
               "megaparsack"
               ["txexpr" #:version "0.2"]
               "curly-fn-lib"
               "functional-lib"))

(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))

(define scribblings '(("scribblings/brazilian-law.scrbl" ())))

(define pkg-desc "Definitions for Brazilian laws in plaintext format. Can parse and print them to other formats.")

(define version "0.1")

(define pkg-authors '("bruno cuconato"))

(define raco-commands
  '(("brazilian-law-parse" (submod brazilian-law/main main) "parse Brazilian law to different formats" #f)))
