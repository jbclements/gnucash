#lang racket

(require "libs.rkt"
         "parse.rkt")


(provide/contract [gnucash-init 
                   (-> 
                    path-string?
                    path-string?
                    void?)])

(provide (all-from-out "libs.rkt"))


(define (gnucash-init gnucash-file zo-file)
  (init-libs (gnucash-read gnucash-file zo-file)))