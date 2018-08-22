#lang curly-fn racket/base

(require
 data/either
 megaparsack
 megaparsack/text
 racket/cmdline
 racket/file
 xml
 )

(require "parser.rkt"
         "print.rkt")

(define (file->output fp output-fn)
  (either (compose1 #{displayln % (current-error-port)}
                    parse-error->string)
          output-fn
          (parse-string law/p (file->string fp #:mode 'text) fp)))

(module+ main
  (define output-format (make-parameter 'xml))

  (define cmd-parser
    (command-line
     #:program "legal-parser"
     #:usage-help 
     "Parse Brazilian laws."
     "---------------------"
     #:once-any
     [("-x" "--xml") "Output XML representation. (DEFAULT)"
                     (output-format 'xml)]
     [("-p" "--pollen") "Output to pollen representation."
                        (output-format 'pollen)]
     [("-s" "--sexp") "Output to S-expression representation."
                      (output-format 'sexp)]
     #:args (filename)

     filename))

  (case (output-format)
    [(xml) (file->output cmd-parser (compose1 displayln xexpr->string))]
    [(pollen) (file->output cmd-parser ->pollen)]
    [(sexp) (file->output cmd-parser write)]))
