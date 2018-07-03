#lang info

(define collection 'multi)

(define deps (list "base"
                   "sxml"
                   "srfi-lib"
                   "srfi-lite-lib"
                   "memoize"
                   "rackunit-lib"
                   "typed-racket-lib"
                   "rackunit-typed"))

(define build-deps
  (list "racket-doc"
        "scribble-lib"))
