#lang typed/racket

(require "parse.rkt"
         "typed-libs.rkt")

;; ensure that the result of gnucash read satisfies TR
#;(define (typed-gnucash-read [gnucash-file : Path-String]
                            [gnucash-cache-file : Path-String])
  : (Listof ))

#;[gnucash-read (-> path-string? path-string? (listof any/c))]