#lang typed/racket/base

(require (for-syntax racket/base)
         racket/match)

(require/typed racket/list
               [add-between (All (T) ((Listof T) T -> (Listof T)))])

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

;; a dataset has an account and an association list mapping times to amounts
;; "Dataset" is a REALLY TERRIBLE NAME for this.
(define-type Dataset (List Account-Sxml (Listof (List time Real))))


(define-predicate gnucash-element? Gnucash-Element)

;; migrating tiny bits from libs.rkt?

(provide id->account
         find-account
         find-accounts/prefix
         account-name-path
         account-type
         parsed->accounts
         parsed->transactions
         transaction-splits
         group-by-account
         account-group->dataset

         account-sxml?
         transaction-sxml?
         
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
         transaction-split-xmls
         transaction-currency
         split-account
         split-value

         colonsep)





;; defines the type Gnucash-XML-Element-Lab


(define-syntax (define-tagged-elt stx)
  (syntax-case stx ()
    [(_ name subtag)
     #`(define-type name (Pairof (quote subtag) (Listof Sxml)))]))

(define-tagged-elt Account-Sxml gnc:account)
(define-predicate account-sxml? Account-Sxml)
(define-predicate account-sxml-list? (Listof Account-Sxml))
(define-tagged-elt Transaction-Sxml gnc:transaction)
(define-predicate transaction-sxml? Transaction-Sxml)
(define-predicate transaction-sxml-list? (Listof Transaction-Sxml))

;; these are the observed types of gnucash elements
(define-type Gnucash-Element-Type
  (U 'book:id 'count:data 'gnc:pricedb 'gnc:commodity 'gnc:account 'gnc:transaction))


;; now that we're using abbreviations these are all silly:
(define book-id-tag 'book:id)
(define count-data-tag 'gnc:count-data)
(define commodity-tag 'gnc:commodity)
(define pricedb-tag 'gnc:pricedb)
(define account-tag 'gnc:account)
(define transaction-tag 'gnc:transaction)

(define date-tag 'ts:date)
(define date-posted-tag 'trn:date-posted)
(define account-name-tag 'act:name)
(define account-parent-tag 'act:parent)
(define account-id-tag 'act:id)
(define account-type-tag 'act:type)
(define transaction-currency-tag 'trn:currency)
(define splits-tag 'trn:splits)
(define split-account-tag 'split:account)
(define split-value-tag 'split:value)

(define dollars
  `(trn:currency
    (cmdty:space "ISO4217")
    (cmdty:id "USD")))

;; given a list of gnucash sxml things, return the transactions:
(define (parsed->transactions [elts : (Listof Gnucash-Element)])
  : (Listof Transaction-Sxml)
  (assert (tag-filter transaction-tag elts) transaction-sxml-list?))

;; given a list of gnucash sxml things, return the accounts:
(define (parsed->accounts [elts : (Listof Gnucash-Element)])
  : (Listof Account-Sxml)
  (assert (tag-filter account-tag elts) account-sxml-list?))


;; organize a list of date-and-splits by account
(define (group-by-account [date-and-splits : Splitlist])
  : (Listof (List String Splitlist))
  (hash-map
   (for/fold
     ([ht : (Immutable-HashTable String Splitlist)
          (hash)])
     ([date-and-split (in-list date-and-splits)])
     (let ([id (split-account (cadr date-and-split))])
       (hash-set ht id (cons date-and-split (hash-ref ht id (λ () `()))))))
   (λ ([s : String] [sl : Splitlist]) (list s sl))))


;; given an account group, produce a dataset...
;; perhaps this should check to make sure it's a dollars transaction?
(define (account-group->dataset [account-group : (List String (Listof (List time Gnucash-Element)))]
                                [accounts : (Listof Account-Sxml)])
  : Dataset
  (list (id->account (car account-group) accounts)
        (for/list ([date-and-split : (List time Gnucash-Element)
                                   (cadr account-group)])
          (list (car date-and-split) (split-value (cadr date-and-split))))))


;; given an id and the list of accounts, return the account
;; referred to by the id
(define (id->account [id : String]
                     [accounts : (Listof Account-Sxml)])
  : Account-Sxml
  (oo/fail (filter (lambda ([account : Account-Sxml])
                     (equal? id (account-id account))) accounts)
           (λ () (format "id->account: no account found for id ~e"
                         id))))


;; find an account with the given name path
(define (find-account [name-path : (Listof String)]
                      [accounts : (Listof Account-Sxml)])
  : Account-Sxml
  (oo/fail (filter (lambda ([acct : Account-Sxml])
                     (equal? (account-name-path acct accounts)
                             name-path))
                   accounts)
           (lambda () (format "no account named ~v" name-path))))


;; find accounts whose name path starts with the given prefix
(define (find-accounts/prefix [name-path : (Listof String)]
                             [accounts : (Listof Account-Sxml)])
  : (Listof Account-Sxml)
  (filter (lambda ([acct : Account-Sxml])
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
(define account-name-path-hash
  : (Mutable-HashTable (Listof Account-Sxml)
                       (Listof String))
  (make-hash))
(define (account-name-path [init-account : Account-Sxml]
                           [accounts : (Listof Account-Sxml)])
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

(define (account-name-path-search [init-account : Account-Sxml]
                                  [accounts : (Listof Account-Sxml)])
  : (Listof String)
  (reverse (let loop : (Listof String)
             ([account : Account-Sxml init-account])
             (let ([maybe-parent (account-parent account)])
               (cons (account-name account)
                     (if maybe-parent
                         (loop (id->account maybe-parent accounts))
                         null))))))

;; return the parent of an account, or #f if it has none
(define (account-parent [account : Account-Sxml]) : (U False String)
  (define maybe-parent-field
    (oof ((sxpath (list account-parent-tag)) account)))
  (cond [(not maybe-parent-field) #f]
        [else
         (match (oo/fail (sxml:content maybe-parent-field)
                         (λ () "bc"))
           [(? string? s) s]
           [other (error 'account-parent
                         "expected string as content of element, got: ~e"
                         other)])]))

(define (account-type [account : Account-Sxml]) : String
  (assert (oo (sxml:content
               (oo ((sxpath (list account-type-tag)) account))))
          string?))

(module+ test
  (check-equal?
   (account-type
    '(gnc:account
      (@ (version "2.0.0"))
      (act:name "Academic")
      (act:id (@ (type "guid")) "0da16f582300ada60adae89f9d275d88")
      (act:type "EXPENSE")
      (act:commodity
       (cmdty:space "ISO4217")
       
       (cmdty:id "USD"))
      (act:commodity-scu "100")
      (act:parent (@ (type "guid")) "ab7ccf91bac526bb8effe6009b97fdfe")))
   "EXPENSE"))

;; returns all the splits of the transaction
(define (transaction-splits [transaction : Gnucash-Element]) : Splitlist
  (let* ([splits (sxml:content (transaction-split-xmls transaction))]
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
  (find-tag/1 account '(act:id)))
  
;; return the splits of a transaction
(: transaction-split-xmls (Sxml -> (Listof Sxml)))
(define (transaction-split-xmls t)
  (sxml:content (find-tag t '(trn:splits))))

;; return the currency of a transaction
(: transaction-currency (Sxml -> Sxml))
(define (transaction-currency t)
  (find-tag t (list transaction-currency-tag)))

(: split-account (Gnucash-Element -> String))
(define (split-account s)
  (assert (find-tag/1 s (list split-account-tag))
          string?))

(define (split-value [s : Gnucash-Element]) : Real
  (assert (string->number
           (assert (find-tag/1 s (list split-value-tag))
                   string?))
          real?))

(define (colonsep [strlist : (Listof String)]) : String
  (apply string-append
         (add-between strlist ":")))

