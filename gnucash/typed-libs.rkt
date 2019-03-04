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
               [string->date (String String -> date)]
               [time<=? (time time -> Boolean)]
               [time<? (time time -> Boolean)])

(require/typed srfi/19/time
               [srfi:make-date (Natural Natural Natural Natural Natural Natural Natural Natural -> date)])


(define-type Sxml (U String Symbol Number (Listof Sxml)))
(define-type Gnucash-Element (Pairof Gnucash-Element-Tag (Listof Sxml)))
(define-type Splitlist (Listof (List time Split)))

;; a dataset has an account and an association list mapping times to amounts
;; "Dataset" is a REALLY TERRIBLE NAME for this.
(define-type Dataset (List Account-Sxml (Listof (List time Real))))

(define-type Transaction (Pairof 'gnc:transaction (Listof Sxml)))
(define-type Split (Pairof 'trn:split (Listof Sxml)))


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

         colonsep

         make-date-filter
         make-year-filter
         year->transactions
         crossers
         net
         external-splits)





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

;; these are the observed types of gnucash elements at the top level
(define-type Gnucash-Top-Element-Tag
  (U 'book:id 'count:data 'gnc:pricedb 'gnc:commodity 'gnc:account 'gnc:transaction))
;; all of the element tags (currently incomplete)
(define-type Gnucash-Element-Tag
  (U 'book:id 'book:slots
     'count:data
     'gnc:pricedb 'gnc:commodity 'gnc:account 'gnc:transaction 'gnc:count-data
     'trn:currency 'trn:date-posted 'trn:splits 'trn:split
     'ts:date
     'act:name 'act:id
     'split:account 'split:value))

(define dollars
  `(trn:currency
    (cmdty:space "ISO4217")
    (cmdty:id "USD")))

;; given a list of gnucash sxml things, return the transactions:
(define (parsed->transactions [elts : (Listof Gnucash-Element)])
  : (Listof Transaction-Sxml)
  (assert (tag-filter 'gnc:transaction elts) transaction-sxml-list?))

;; given a list of gnucash sxml things, return the accounts:
(define (parsed->accounts [elts : (Listof Gnucash-Element)])
  : (Listof Account-Sxml)
  (assert (tag-filter 'gnc:account elts) account-sxml-list?))


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
    (oof ((sxpath (list 'act:parent)) account)))
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
               (oo ((sxpath (list 'act:type)) account))))
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
  (let* ([splits (cast (sxml:content (transaction-split-xmls transaction))
                       (Listof Split))]
         [date (transaction-date transaction)])
    (for/list ([split (in-list splits)])
      (list (date->time-utc date) split))))


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
(: find-tag (Sxml (Listof Gnucash-Element-Tag) -> Sxml))
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
(: find-tag/1 (Sxml (Listof Gnucash-Element-Tag) -> Sxml))
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
    (find-tag/1 transaction (list 'trn:date-posted 'ts:date)))
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
  (ensure-string (oo (sxml:content (find-tag account (list 'act:name))))))

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
  (find-tag t (list 'trn:currency)))

(: split-account (Gnucash-Element -> String))
(define (split-account s)
  (assert (find-tag/1 s (list 'split:account))
          string?))

(define (split-value [s : Gnucash-Element]) : Real
  (assert (string->number
           (assert (find-tag/1 s (list 'split:value))
                   string?))
          real?))

(define (colonsep [strlist : (Listof String)]) : String
  (apply string-append
         (add-between strlist ":")))




;; date date -> (transaction -> boolean)
(define (make-date-filter [start : date] [end : date])
  (lambda ([transaction : Sxml])
    (let ([ttime (date->time-utc (transaction-date transaction))]
          [stime (date->time-utc start)]
          [etime (date->time-utc end)])
      (and (time<=? stime ttime)
           (time<? ttime etime)))))


(define (make-year-filter [year : Natural])
  (make-date-filter (srfi:make-date 0 0 0 0 1 1 year 0)
                    (srfi:make-date 0 0 0 0 1 1 (+ year 1) 0)))

;; given a year and the set of all transactions
(define (year->transactions [year : Natural] [transactions : (Listof Sxml)])
  (filter (make-year-filter year) transactions))

  
;; find all transactions where at least one split is in the list of 
;; account ids and one split is outside the list.
(define (crossers [transactions : (Listof Transaction)]
                  [account-ids : (Listof String)])
  (filter (lambda ([transaction : Transaction])
            (let ([split-account-ids (map split-account
                                          (cast (sxml:content (transaction-split-xmls transaction))
                                                (Listof Gnucash-Element)))])
              (and (ormap (lambda ([id : String]) (member id account-ids))
                          split-account-ids)
                   (ormap (lambda ([id : String]) (not (member id account-ids)))
                          split-account-ids))))
          transactions))


  

  
;; compute the net of the transaction w.r.t. the given accounts.
(define (net [transaction : Transaction]
             [acct-ids : (Listof String)]
             [currency : Gnucash-Element])
  (unless (equal? (transaction-currency transaction) currency)
    (error 'net "transaction has wrong currency; expected ~v, got ~v" currency (transaction-currency transaction)))
  (let ([splits (cast (sxml:content (transaction-split-xmls transaction))
                      (Listof Gnucash-Element))])
    (foldl + 0
           (map split-value
                (filter (lambda ([s : Gnucash-Element])
                          (not (member (split-account s) acct-ids)))
                        splits)))))


;; returns the splits of the transaction that do not involve the given accounts
(define (external-splits [transaction : Transaction]
                         [account-ids : (Listof String)])
  (let* ([splits (cast (sxml:content (transaction-split-xmls transaction))
                       (Listof Gnucash-Element))]
         [date (transaction-date transaction)]
         [externals (filter (lambda ([s : Gnucash-Element])
                              (not (member (split-account s) account-ids)))
                            splits)])
    (map (lambda ([split : Gnucash-Element]) (list (date->time-utc date) split)) externals)))

(module+ test
  (require typed/rackunit)

  (define sample-transactions
    '((gnc:transaction
       (@ (version "2.0.0"))
       (trn:id
        (@ (type "guid"))
        "3df712935ab9db8f645058ec2d3d6bc0")
       (trn:currency (cmdty:space "CURRENCY") (cmdty:id "USD"))
       (trn:date-posted (ts:date "2003-06-19 03:59:00 -0700"))
       (trn:date-entered (ts:date "2003-06-29 20:31:29 -0700"))
       (trn:description "ATM 101.25")
       (trn:splits
        (trn:split
         (split:id
          (@ (type "guid"))
          "49f5f12f66cf8e9b38358f671cd9f466")
         (split:reconciled-state "n")
         (split:value "10125/100")
         (split:quantity "10125/100")
         (split:account
          (@ (type "guid"))
          "21d12468be04ef2b4ddb3cfbd965c86a"))
        (trn:split
         (split:id
          (@ (type "guid"))
          "5c1264d967f259c200a2122b8749d3b9")
         (split:reconciled-state "y")
         (split:reconcile-date
          (ts:date "2003-06-29 21:00:00 -0700"))
         (split:value "-10125/100")
         (split:quantity "-10125/100")
         (split:account
          (@ (type "guid"))
          "6889db532d4378952fc19cf771566441"))))
      (gnc:transaction
       (@ (version "2.0.0"))
       (trn:id
        (@ (type "guid"))
        "c01e92077cbc65aff35a445e01ee1f1d")
       (trn:currency (cmdty:space "CURRENCY") (cmdty:id "USD"))
       (trn:date-posted (ts:date "2003-06-20 03:59:00 -0700"))
       (trn:date-entered (ts:date "2003-06-27 05:28:52 -0700"))
       (trn:description "City of Newton / Water")
       (trn:splits
        (trn:split
         (split:id
          (@ (type "guid"))
          "7a5986490d3be0e15293968aba394711")
         (split:reconciled-state "n")
         (split:value "4508/100")
         (split:quantity "4508/100")
         (split:account
          (@ (type "guid"))
          "475c9fe50f47a33910973217af155da3"))
        (trn:split
         (split:id
          (@ (type "guid"))
          "c3e68cc175fa1a17f6852764635a7186")
         (split:reconciled-state "y")
         (split:reconcile-date
          (ts:date "2003-06-29 21:00:00 -0700"))
         (split:value "-4508/100")
         (split:quantity "-4508/100")
         (split:account
          (@ (type "guid"))
          "6889db532d4378952fc19cf771566441"))))))

  (check-equal?
   (crossers sample-transactions '("21d12468be04ef2b4ddb3cfbd965c86a"))
   (list (car sample-transactions)))

  (check-equal?
   (crossers sample-transactions '("6889db532d4378952fc19cf771566441"
                                   "21d12468be04ef2b4ddb3cfbd965c86a"))
   (list (cadr sample-transactions)))

  (check-equal?
   (crossers sample-transactions '("6889db532d4378952fc19cf771566441"
                                   "21d12468be04ef2b4ddb3cfbd965c86a"
                                   "475c9fe50f47a33910973217af155da3"))
   '())

  
  (check-=
   (net (car sample-transactions) '("21d12468be04ef2b4ddb3cfbd965c86a")
        '(trn:currency (cmdty:space "CURRENCY")
                       (cmdty:id "USD")))
   -101.25
   1e-5)

  
  (check-equal?
   (external-splits (car sample-transactions)
                    '("6889db532d4378952fc19cf771566441"))
   (list
    (list
     (date->time-utc
      (transaction-date (car sample-transactions)))
     '(trn:split
       (split:id
        (@ (type "guid"))
        "49f5f12f66cf8e9b38358f671cd9f466")
       (split:reconciled-state "n")
       (split:value "10125/100")
       (split:quantity "10125/100")
       (split:account
        (@ (type "guid"))
        "21d12468be04ef2b4ddb3cfbd965c86a")))
     ))
  )