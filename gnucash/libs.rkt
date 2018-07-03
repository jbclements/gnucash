#lang racket/base

(require sxml
         srfi/19/time
         racket/match
         "typed-libs.rkt"
         memoize
         rackunit
         racket/contract)
  
;; an account is an sxml datum. It looks like this. Ooh, I'd forgotten
;; how much I hated XML...
#;'(http://www.gnucash.org/XML/gnc:account
  (@ (version "2.0.0"))
  (http://www.gnucash.org/XML/act:name "Root Account")
  (http://www.gnucash.org/XML/act:id
   (@ (type "guid"))
   "ab7ccf91bac526bb8effe6009b97fdfe")
  (http://www.gnucash.org/XML/act:type "ROOT"))
;; an account has a version attribute, and these elements:
;; - name (string)
;; - id (attribute type) (string)
;; - type
;; - optional parent
;; - optional currency
;; - optional commodity-scu
;; - optional slots

;; a transaction is an xml element



;; this provide is way too coarse, but I can't be bothered to fix it.
(provide (except-out (all-defined-out)
                     group-by-account
                     account-group->dataset)
         (contract-out 
          [group-by-account (-> splitlist/c
                                (listof
                                 (list/c id-string? 
                                         splitlist/c)))]
          [account-group->dataset
           (-> (list/c id-string? splitlist/c)
               dataset/c)
]))

(define transaction? list?)
(define account? list?)
;; a split is part of a transaction, showing money
;; moving from one account to another.
(define split? list?)
(define id-string? string?)
;; a splitlist is an association from time to split
(define splitlist/c (listof (list/c time? split?)))
;; a dataset has an account and an association list mapping times to amounts
(define dataset/c (list/c account? (listof (list/c time? number?))))

  
(define (split-value s)
  (string->number (find-tag/1 s (list split-value-tag))))
  


;; an account-tree is
;; - (make-acct-tree name acct (listof account-tree)


  


  

;; date date -> (transaction -> boolean)
(define (make-date-filter start end)
  (lambda (transaction)
    (let ([ttime (date->time-utc (transaction-date transaction))]
          [stime (date->time-utc start)]
          [etime (date->time-utc end)])
      (and (time<=? stime ttime)
           (time<? ttime etime)))))
  

(define (make-year-filter year)
  (make-date-filter (srfi:make-date 0 0 0 0 1 1 year 0)
                    (srfi:make-date 0 0 0 0 1 1 (+ year 1) 0)))

;; given a year and the set of all transactions
(define (year->transactions year transactions)
  (filter (make-year-filter year) transactions))
  
;; find all transactions where at least one split is in the list of 
;; account ids and one split is outside the list.
(define (crossers transactions account-ids)
  (filter (lambda (transaction)
            (let ([split-account-ids (map split-account (sxml:content (transaction-splits transaction)))])
              (and (ormap (lambda (id) (member id account-ids))
                          split-account-ids)
                   (ormap (lambda (id) (not (member id account-ids)))
                          split-account-ids))))
          transactions))
  
  ;; compute the net of the transaction w.r.t. the given accounts.
  (define (net transaction acct-ids currency)
    (unless (equal? (transaction-currency transaction) currency)
      (error 'net "transaction has wrong currency; expected ~v, got ~v" currency (transaction-currency transaction)))
    (let ([splits (sxml:content (transaction-splits transaction))])
      (foldl + 0 (map split-value (filter (lambda (s) (not (member (split-account s) acct-ids))) splits)))))
  
  ;; returns the splits of the transaction that do not involve the given accounts
  (define (external-splits transaction account-ids)
    (let* ([splits (sxml:content (transaction-splits transaction))]
           [date (transaction-date transaction)]
           [externals (filter (lambda (s) (not (member (split-account s) account-ids))) splits)])
      (map (lambda (split) (list (date->time-utc date) split)) externals)))
  
  (define (print-transaction t)
    (printf "~a\n" (date->string (transaction-date t)))
    (unless (equal? (transaction-currency t) dollars)
      (printf "NON-DOLLAR TRANSACTION\n"))
    (for-each print-split (transaction-splits t)))
  
  (define (print-split s)
    (printf "~v : ~v\n" (account-name-path (id->account (split-account s))) (split-value s)))
  
  

;; ********
  
(define (jan-one year) (srfi:make-date 0 0 0 0 1 1 year 0))
(define (feb-one year) (srfi:make-date 0 0 0 0 1 2 year 0))
(define (mar-one year) (srfi:make-date 0 0 0 0 1 3 year 0))
(define (apr-one year) (srfi:make-date 0 0 0 0 1 4 year 0))
(define (may-one year) (srfi:make-date 0 0 0 0 1 5 year 0))
(define (jun-one year) (srfi:make-date 0 0 0 0 1 6 year 0))
(define (jul-one year) (srfi:make-date 0 0 0 0 1 7 year 0))
(define (aug-one year) (srfi:make-date 0 0 0 0 1 8 year 0))
(define (sep-one year) (srfi:make-date 0 0 0 0 1 9 year 0))
(define (oct-one year) (srfi:make-date 0 0 0 0 1 10 year 0))
(define (nov-one year) (srfi:make-date 0 0 0 0 1 11 year 0))
(define (dec-one year) (srfi:make-date 0 0 0 0 1 12 year 0))


  
  
;; organize a list of date-and-splits by account
(define (group-by-account date-and-splits)
  (hash-map
   (for/fold ([ht (hash)])
             ([date-and-split (in-list date-and-splits)])
     (let ([id (split-account (cadr date-and-split))])
       (hash-set ht id (cons date-and-split (hash-ref ht id `())))))
   list))

(define (generate-budget-report grouped)
  (map (match-lambda [(list id splits) 
                      (list (account-name-path (id->account id))
                            (apply + (map split-value (map cadr splits))))])
       grouped))

(define (budget-report s e accounts transactions)
  (generate-budget-report (splits-by-account s e (map account-id accounts))))
  
(define (splits-by-account s e acct-ids transactions)
  (let* ([crossers (crossers (transactions-in-range s e transactions) acct-ids)]
         [external-motion (apply append (map (lambda (transaction)
                                               (external-splits transaction acct-ids))
                                             crossers))])
    (group-by-account external-motion)))
  
(define (transactions-in-range s e transactions)
  (filter (make-date-filter s e) transactions))
  
(define (pair-up a b)
  (let ([ht (make-hash)])
    (for-each (match-lambda 
                [(list k v) (hash-set! ht k (list v))])
              a)
    (for-each (match-lambda 
                [(list k v) (hash-set! ht k (cons v (hash-ref ht k (list 0))))])
              b)
    (hash-map ht (lambda (k v) (match v 
                                 [(list a b) (list k b a)]
                                 [(list a) (list k a 0)])))))
  
  (define (expenses-only br)
    (filter (match-lambda [(list name a b)
                           (cond [(and (>= a 0) (>= b 0))
                                  #t]
                                 [(or (> a 0) (> b 0))
                                  (error 'expenses-only "account ~v has mixed-sign numbers: ~v and ~v" name a b)]
                                 [else #f])])
            br))
  
(define (print-it a)
  (for-each (match-lambda [(list name a b) (printf "~a\t~v\t~v\n" (colonsep name) (digfmt a) (digfmt b))]) a))
  


;; i think this could be replaced by ~r...
(define (digfmt n)
  (/ (* n 100) 100.0))


  

;; given an account group, produce a dataset...
;; perhaps this should check to make sure it's a dollars transaction?
(define (account-group->dataset account-group)
  (list (id->account (car account-group))
        (for/list ([date-and-split (cadr account-group)])
          (list (car date-and-split) (split-value (cadr date-and-split))))))