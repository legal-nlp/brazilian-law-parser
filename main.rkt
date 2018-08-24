#lang curly-fn racket/base

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included a LICENSE.txt file, which links to
;; the GNU Lesser General Public License.
;; If you would prefer to use a different license, replace LICENSE.txt with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here
(module+ test
  #;(require rackunit))

(module+ main
  (require
   data/either
   megaparsack
   megaparsack/text
   racket/cmdline
   racket/file
   txexpr
   xml
   )
  
  (require brazilian-law-parser/parser
           brazilian-law-parser/print)
  
  (define (file->output fp output-fn)
    (either (compose1 #{displayln % (current-error-port)}
                      parse-error->string)
            output-fn
            (parse-string law/p (file->string fp #:mode 'text) fp)))
  
  (define output-format (make-parameter 'xml))
  
  (define cmd-parser
    (command-line
     #:program "legal-parser"
     #:usage-help 
     "Parse Brazilian laws to several output formats."
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
    [(pollen) (file->output cmd-parser (compose1 ->pollen get-elements))]
    [(sexp) (file->output cmd-parser write)]))
