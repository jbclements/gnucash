#lang typed/racket/base

(require racket/match
         memoize)

(require/typed sxml
               [sxpath (Any -> (Sxml -> (Listof Sxml)))]
               [sxml:content (Sxml -> (Listof Sxml))])

(require/typed srfi/19
               [#:opaque date date?]
               [#:opaque time time?]
               [date->time-utc (date -> time)]
               [string->date (String String -> date)])

(define-type Sxml (U String Symbol Number (Listof Sxml)))
(define-type Gnucash-Element (Pairof Symbol (Listof Sxml)))
(define-type Splitlist (Listof (List time Gnucash-Element)))

(define-predicate gnucash-element? Gnucash-Element)

;; migrating tiny bits from libs.rkt?

(provide gnucash-element-type
         Gnucash-Element-Type
         id->account
         find-account
         find-account/prefix
         account-name-path
         parsed->accounts
         parsed->transactions
         all-splits
         
         book-id-tag
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

(define gnucash-stem "http://www.gnucash.org/")
(define gnucash-xml-stem (string-append gnucash-stem "XML/"))
(define (gnucash-xml-label [id : Symbol]) : Symbol
  (string->symbol (string-append gnucash-xml-stem (symbol->string id))))

;; these are the observed types of gnucash elements
(define-type Gnucash-Element-Type
  (U 'book-id 'count-data 'pricedb 'commodity 'account 'transaction))

(define book-id-tag (gnucash-xml-label 'book:id))
(define count-data-tag (gnucash-xml-label 'gnc:count-data))
;; may have changed?
(define commodity-tag
  (gnucash-xml-label 'gnc:commodity)
  #;(string->symbol "http://www.gnucash.org/lxr/gnucash/source/src()/doc/xml/io-gncxml-version-2.dtd#gnc:commodity"))
(define pricedb-tag (gnucash-xml-label 'gnc:pricedb))
(define account-tag (gnucash-xml-label 'gnc:account))
(define transaction-tag (gnucash-xml-label 'gnc:transaction))

(define date-tag (gnucash-xml-label '|ts:date|))
(define date-posted-tag (gnucash-xml-label '|trn:date-posted|))
(define account-name-tag (gnucash-xml-label '|act:name|))
(define account-parent-tag (gnucash-xml-label 'act:parent))
(define account-id-tag (gnucash-xml-label 'act:id))
(define transaction-currency-tag (gnucash-xml-label 'trn:currency))
(define splits-tag (gnucash-xml-label 'trn:splits))
(define split-account-tag (gnucash-xml-label 'split:account))
(define split-value-tag (gnucash-xml-label '|split:value|))

(define dollars
  `(http://www.gnucash.org/XML/trn:currency
    (http://www.gnucash.org/XML/cmdty:space "ISO4217")
    (http://www.gnucash.org/XML/cmdty:id "USD")))

;; gnucash elements have different tags; these should be ex...
(define (gnucash-element-type [element : (Pairof Symbol
                                                 (Listof Sxml))])
  : Gnucash-Element-Type
  (hash-ref element-type-table (car element)
            (λ () (error 'gnucash-element-type
                         "unexpected sxml tag: ~e"
                         (car element)))))

(define element-type-table : (HashTable Symbol Gnucash-Element-Type)
  (make-immutable-hash
   (ann
    (list
     (cons book-id-tag 'book-id)
     (cons count-data-tag 'count-data)
     (cons commodity-tag 'commodity)
     (cons account-tag 'account)
     (cons transaction-tag 'transaction))
    (Listof (Pairof Symbol Gnucash-Element-Type)))))

;; given a list of gnucash sxml things, return the transactions:
(define (parsed->transactions [elts : (Listof Gnucash-Element)])
  (tag-filter transaction-tag elts))

;; given a list of gnucash sxml things, return the accounts:
(define (parsed->accounts [elts : (Listof Gnucash-Element)])
  (tag-filter account-tag elts))

;; given an id and the list of accounts, return the account
;; referred to by the id
(define (id->account [id : String]
                     [accounts : (Listof Gnucash-Element)])
  (oo (filter (lambda ([account : Gnucash-Element])
                (equal? id (account-id account))) accounts)))


;; find an account with the given name path
(define (find-account [name-path : (Listof String)]
                      [accounts : (Listof Gnucash-Element)])
  (oo/fail (filter (lambda ([acct : Gnucash-Element])
                     (equal? (account-name-path acct accounts)
                             name-path))
                   accounts)
           (lambda () (format "no account named ~v" name-path))))


;; find accounts whose name path starts with the given prefix
(define (find-account/prefix [name-path : (Listof String)]
                             [accounts : (Listof Gnucash-Element)])
  (filter (lambda ([acct : Gnucash-Element])
            (prefix? name-path (account-name-path acct accounts)))
          accounts))


;; list list -> boolean
;; is 'a' a prefix of 'b' ?
(: prefix? (All (T) ((Listof T) (Listof T) -> Boolean)))
(define (prefix? a b)
  (match (list a b)
    [(list (list) any) #t]
    [(list (cons a arest) (cons b brest)) (and (equal? a b) (prefix? arest brest))]
    [else #f]))

(module+ test
  (require typed/rackunit)
  (check-true (prefix? `() `()))
  (check-true (prefix? `(a) `(a)))
  (check-false (prefix? `(a b) `(a c)))
  (check-true (prefix? `(a b c) `(a b c d))))


;; given a gnucash-element representing an account, return
;; a list of strings representing the name chain, e.g.
;; '("Root Account" "Assets" "Jewelry")
;; memoization here is totally vital
;; gee whiz... I wish memoize worked on typed racket. sigh.
(define account-name-path-hash
  : (Mutable-HashTable (Listof Gnucash-Element)
                       (Listof String))
  (make-hash))
(define (account-name-path [init-account : Gnucash-Element]
                           [accounts : (Listof Gnucash-Element)])
  : (Listof String)
  (define key (cons init-account accounts))
  (hash-ref
   account-name-path-hash
   key
   (λ ()
     (define result (account-name-path-search init-account
                                              accounts))
     (hash-set! account-name-path-hash key result)
     result)))

(define (account-name-path-search [init-account : Gnucash-Element]
                                  [accounts : (Listof Gnucash-Element)])
  : (Listof String)
  (reverse (let loop : (Listof String)
             ([account : Gnucash-Element init-account])
             (let ([maybe-parent (account-parent account)])
               (cons (account-name account)
                     (if maybe-parent
                         (loop (id->account maybe-parent accounts))
                         null))))))

;; return the parent of an account, or #f if it has none
(define (account-parent [account : Gnucash-Element]) : (U False String)
  (define maybe-parent-field
    (oof ((sxpath (list account-parent-tag)) account)))
  (cond [(not maybe-parent-field) #f]
        [else
         (match (oo (sxml:content maybe-parent-field))
           [(? string? s) s]
           [other (error 'account-parent
                         "expected string as content of elemnt, got: ~e"
                         other)])]))



;; returns all the splits of the transaction
(define (all-splits [transaction : Gnucash-Element]) : Splitlist
  (let* ([splits (sxml:content (transaction-splits transaction))]
         [date (transaction-date transaction)])
    (map (lambda ([split : Sxml])
           (list (date->time-utc date) (assert split gnucash-element?)))
         splits)))


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
  (filter (lambda ([elt : (Pair T U)])
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