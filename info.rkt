#lang info

(define collection "brazilian-law-parser")

(define deps '(["base" #:version "6.5"]
               "megaparsack"
               ["txexpr" #:version "0.2"]))

(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))

(define scribblings '(("scribblings/brazilian-law-parser.scrbl" ())))

(define pkg-desc "Parses Brazilian laws in plaintext format and prints them to other formats.")

(define version "0.1")

(define pkg-authors '(bruno))

(define raco-commands
  '(("brazilian-law-parse" (submod brazilian-law-parser/main main) "parse Brazilian to different formats" #f)))
