#lang racket/base

(require
 data/either
 megaparsack
 megaparsack/text
 racket/cmdline
 racket/file
 )

(require "parser.rkt"
         "print.rkt")

(define output-format (make-parameter 'raw))

(define cmd-parser
  (command-line
   #:program "legal-parser"
   #:usage-help 
   "Parse Brazilian laws."
   "---------------------"
   #:once-any
   [("-p" "--pollen") "Output to pollen representation"
                      (output-format 'pollen)]
   [("-r" "--raw") "Output parser representation"
                   (output-format 'raw)]
   #:args (filename)

   filename))

(define (file->output fp output-fn)
  (either (compose1 displayln parse-error->string)
          output-fn
          (parse-string law/p (file->string fp #:mode 'text) fp)))

(case (output-format)
  [(pollen) (file->output cmd-parser ->pollen)]
  [(raw) (file->output cmd-parser displayln)]
  [else (displayln "ichi")])
