#lang typed/racket/base

(require racket/match
         )

(require/typed sxml
               [sxpath (Any -> (Sxml -> (Listof Sxml)))]
               [sxml:content (Sxml -> (Listof Sxml))])

(require/typed srfi/19
               [#:opaque date date?]
               [string->date (String String -> date)])

(define-type Sxml (U String Symbol Number (Listof Sxml)))

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
         tag-filter
         find-tag
         find-tag/1
         transaction-date
         account-name
         account-id
         transaction-splits
         transaction-currency
         split-account)

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

;; return first of list of length one, use fail to generate error otherwise
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

;; find a given tag, signal an error if missing or more than one
(: find-tag (Sxml (Listof Symbol) -> Sxml))
(define (find-tag elt tag-list)
  (define proc (sxpath tag-list))
  (unless (procedure? proc)
    (raise-argument-error 'find-tag
                          "tag-list that works with sxpath"
                          1 elt tag-list))
  (oo/fail (proc elt)
           (lambda ()
             (raise-argument-error 'find-tag
                                   (format "element with tags ~v" tag-list)
                                   0 elt tag-list))))

;; find the single element in the given tag
(: find-tag/1 (Sxml (Listof Symbol) -> Sxml))
(define (find-tag/1 elt tag-list)
  (oo/fail (sxml:content (find-tag elt tag-list))
           (lambda ()
             (raise-argument-error 'find-tag
                                   (format 
                                    "element with tags ~v containing exactly one element"
                                    tag-list)
                                   0 elt tag-list))))


; given a transaction, return its date.
(: transaction-date (Sxml -> date))
(define (transaction-date transaction) 
  (string->date
   (ensure-string
    (find-tag/1 transaction (list date-posted-tag date-tag)))
   "~Y-~m-~d ~H:~M:~S ~z"))

(: ensure-string (Any -> String))
(define (ensure-string str)
  (cond [(string? str) str]
        [else (raise-argument-error 'ensure-string
                                    "string"
                                    0 str)]))

;; given an account, return its name
(: account-name (Sxml -> String))
(define (account-name account)
  (ensure-string (oo (sxml:content (find-tag account (list account-name-tag))))))

;; return the id of an account
(: account-id (Sxml -> Sxml))
(define (account-id account)
  (find-tag/1 account (list account-id-tag)))
  
;; return the splits of a transaction
(: transaction-splits (Sxml -> (Listof Sxml)))
(define (transaction-splits t)
  (sxml:content (find-tag t (list splits-tag))))

;; return the currency of a transaction
(: transaction-currency (Sxml -> Sxml))
(define (transaction-currency t)
  (find-tag t (list transaction-currency-tag)))

(: split-account (Sxml -> Sxml))
(define (split-account s)
  (find-tag/1 s (list split-account-tag)))