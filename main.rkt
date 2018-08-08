#lang racket/base

(require
 data/either
 megaparsack
 megaparsack/text
 racket/cmdline
 racket/file
 )

(require "parser.rkt")

(define input-format (make-parameter 'human))

(define cmd-parser
  (command-line
   #:program "legal-parser"
   #:usage-help 
   "Parse Brazilian laws."
   "---------------------"
   
   #:args (filename)

   filename))

(define (file->law fp)
  (either (compose1 displayln parse-error->string)
          displayln
          (parse-string law/p (file->string fp #:mode 'text) fp)))

(file->law cmd-parser)
