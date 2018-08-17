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

(define (file->output fp output-fn)
  (either (compose1 #{displayln % (current-output-port)}
                    parse-error->string)
          output-fn
          (parse-string law/p (file->string fp #:mode 'text) fp)))

(module+ main
  (define output-format (make-parameter 'sexp))

  (define cmd-parser
    (command-line
     #:program "legal-parser"
     #:usage-help 
     "Parse Brazilian laws."
     "---------------------"
     #:once-any
     [("-p" "--pollen") "Output to pollen representation"
                        (output-format 'pollen)]
     [("-s" "--sexp") "Output parser sexp representation"
                      (output-format 'sexp)]
     #:args (filename)

     filename))

  (case (output-format)
    [(pollen) (file->output cmd-parser ->pollen)]
    [(sexp) (file->output cmd-parser displayln)]))
