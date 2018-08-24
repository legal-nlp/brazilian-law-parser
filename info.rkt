#lang info

(define collection "brazilian-law-parser")

(define deps '("base" "megaparsack" "txexpr"))

(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))

(define scribblings '(("scribblings/brazilian-law-parser.scrbl" ())))

(define pkg-desc "Parses Brazilian laws in plaintext format and prints them to other formats.")

(define version "0.1")

(define pkg-authors '(bruno))
