#lang info

(define collection 'multi)

(define deps (list "base"
                   "sxml"
                   "srfi-lib"
                   "srfi-lite-lib"
                   "memoize"
                   "rackunit-lib"
                   "typed-racket-lib"
                   "unstable-contract-lib"))

(define build-deps
  (list "racket-doc"
        "scribble-lib"))
