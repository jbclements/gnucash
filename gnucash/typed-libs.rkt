#lang typed/racket/base

(require racket/match)

;; migrating tiny bits from libs.rkt?

(provide book-id-tag
         count-data-tag
         commodity-tag
         pricedb-tag
         account-tag
         transaction-tag
         date-tag
         date-posted-tag
         account-name-tag
         account-parent-tag
         account-id-tag
         transaction-currency-tag
         splits-tag
         split-account-tag
         split-value-tag
         dollars
         oo
         oo/fail
         oof
         tag-filter)

(define book-id-tag (string->symbol "http://www.gnucash.org/XML/book:id"))
(define count-data-tag (string->symbol "http://www.gnucash.org/XML/gnc:count-data"))
(define commodity-tag (string->symbol "http://www.gnucash.org/lxr/gnucash/source/src()/doc/xml/io-gncxml-version-2.dtd#gnc:commodity"))
(define pricedb-tag (string->symbol "http://www.gnucash.org/XML/gnc:pricedb"))
(define account-tag (string->symbol "http://www.gnucash.org/XML/gnc:account"))
(define transaction-tag (string->symbol "http://www.gnucash.org/XML/gnc:transaction"))

(define date-tag '|http://www.gnucash.org/XML/ts:date|)
(define date-posted-tag '|http://www.gnucash.org/XML/trn:date-posted|)
(define account-name-tag '|http://www.gnucash.org/XML/act:name|)
(define account-parent-tag '|http://www.gnucash.org/XML/act:parent|)
(define account-id-tag '|http://www.gnucash.org/XML/act:id|)
(define transaction-currency-tag 'http://www.gnucash.org/XML/trn:currency)
(define splits-tag '|http://www.gnucash.org/XML/trn:splits|)
(define split-account-tag '|http://www.gnucash.org/XML/split:account|)
(define split-value-tag 'http://www.gnucash.org/XML/split:value)

(define dollars
  `(http://www.gnucash.org/XML/trn:currency
    (http://www.gnucash.org/XML/cmdty:space "ISO4217")
    (http://www.gnucash.org/XML/cmdty:id "USD")))


;; return elt for lists of length one
(: oo (All (T) ((Listof T) -> T)))
(define oo
  (case-lambda
    ((x) (match x
           [(list elt) elt]
           [any (error 'oo "expected list of length one, got: ~v" any)]))
    ))

(: oo/fail (All (T) ((Listof T) (-> String) -> T)))
(define (oo/fail x fail)
  (match x
    [(list elt) elt]
    [any (error 'oo (fail))]))


  
;; return a single element or #f if empty
(: oof (All (T) ((Listof T) -> (U False T))))
(define (oof x)
  (match x
    [(list elt) elt]
    [(list) #f]
    [any (error 'oo "expected list of length one or zero, got: ~v" any)]))

 
;; return only those elements whose car is eq? to the tag
(: tag-filter (All (T U) (T (Listof (Pair T U)) -> (Listof (Pair T U)))))
(define (tag-filter tag elts)
  (filter (lambda: ([elt : (Pair T U)])
            (eq? (car elt) tag))
          elts))