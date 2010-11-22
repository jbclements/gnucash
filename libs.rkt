#lang racket
  
  (require (planet lizorkin/sxml:2)
           (lib "time.ss" "srfi" "19")
           (planet dherman/memoize))
  
  ;; this provide is way too coarse, but I can't be bothered to fix it.
  (provide (all-defined-out))

  ;; this explicit init is gross, but fixing it would require going to units.
  (define book-ids #f)
  (define count-data #f)
  (define commodities #f)
  (define accounts #f)
  (define transactions #f)
  
  (define (init-libs list-of-things)
    (set! book-ids (tag-filter book-id-tag list-of-things))
    (set! count-data (tag-filter count-data-tag list-of-things))
    (set! commodities (tag-filter commodity-tag list-of-things))
    (set! accounts (tag-filter account-tag list-of-things))
    (set! transactions (tag-filter transaction-tag list-of-things)))
  
  
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
  
  (define (tag-filter tag elts)
    (filter (lambda (elt)
              (eq? (car elt) tag))
            elts))

  
  ;; return elt for lists of length one
  (define oo
    (case-lambda 
      ((x) (match x
             [(list elt) elt]
             [any #;(error 'oo "zap")
                  (error 'oo "expected list of length one, got: ~v" any)]))
      ((x fail) (match x
             [(list elt) elt]
             [any (error 'oo (fail))]))))
  
  ;; return true for lists of length one or zero
  (define (oof x)
    (match x
      [(list elt) elt]
      [(list) #f]
      [any (error 'oo "expected list of length one or zero, got: ~v" any)]))
  
  (define (transaction-date transaction) 
    (string->date (oo (sxml:content (oo ((sxpath (list date-posted-tag date-tag)) transaction)))) "~Y-~m-~d ~H:~M:~S ~z"))
  
  (define (account-name account)
    (match (sxml:content (oo ((sxpath (list account-name-tag)) account)))
      [(list) #f]
      [(list name) name]
      [any (error 'account-parent "expected one or zero names, got: ~v" any)]))
  
  (define (account-parent account)
    (match ((sxpath (list account-parent-tag)) account)
      [(list parent) (oo (sxml:content parent))]
      [(list) #f]
      [any (error 'account-parent "expected one or zero parents, got: ~v" any)]))
  
  (define (account-id account)
    (oo (sxml:content (oo ((sxpath (list account-id-tag)) account) 
                          (lambda () (format "account-id-tag not found in: ~v" account))))
        (lambda () (format "content of account-id-tag not found in: ~v" account))))
  
  (define (transaction-splits t)
    (sxml:content (oo ((sxpath (list splits-tag)) t)
                      (lambda () (format "transaction has no splits: ~v" t)))))
  
  (define (transaction-currency t)
    (oo ((sxpath (list transaction-currency-tag)) t)))
  
  (define (split-account s)
    (oo (sxml:content (oo ((sxpath (list split-account-tag)) s)))))
  
  (define (split-value s)
    (string->number (oo (sxml:content (oo ((sxpath (list split-value-tag)) s))))))
  
  (define (id->account id)
    (oo (filter (lambda (account) (string=? id (account-id account))) accounts)))
  
  (define/memo (account-name-path account)
    (reverse (let loop ([account account])
               (let ([maybe-parent (account-parent account)])
                 (cons (account-name account)
                       (if maybe-parent
                           (loop (id->account maybe-parent))
                           null))))))
  
  (define (find-account name-path)
    (oo (filter (lambda (acct) (equal? (account-name-path acct) name-path))
            accounts)
        (lambda () (format "no account named ~v" name-path))))
  
  (define (find-account/prefix name-path)
    (filter (lambda (acct) (prefix? name-path (account-name-path acct)))
            accounts))
  
  (define (prefix? a b)
    (match (list a b)
      [(list (list) any) #t]
      [(list (list-rest a arest) (list-rest b brest)) (and (equal? a b) (prefix? arest brest))]
      [else #f]))
  
  #;(begin (>>> (prefix? `() `()))
  (>>> (prefix? `(a) `(a)))
  (>>> (not (prefix? `(a b) `(a c))))
  (>>> (prefix? `(a b c) `(a b c d))))
  
  
  ;; returns all the splits of the transaction
  (define (all-splits transaction)
    (let* ([splits (sxml:content (transaction-splits transaction))]
           [date (transaction-date transaction)])
      (map (lambda (split) (list (date->time-utc date) split)) splits)))
  

  
  
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
  
  (define (year->transactions year) (filter (make-year-filter year) transactions))
  
  ;; find all transactions where at least one split is in the list of account ids and one split is outside the list.
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

    
  (define (group-by-account date-and-splits)
    (let* ([ht (make-hash)])
      (for-each (lambda (date-and-split) 
                  (let ([id (split-account (cadr date-and-split))])
                    (hash-set! ht id (cons date-and-split (hash-ref ht id (lambda () `())))))) 
                date-and-splits)
      (hash-map ht list)))
  
  (define (generate-budget-report grouped)
    (map (match-lambda [(list id splits) (list (account-name-path (id->account id)) (apply + (map split-value (map cadr splits))))]) grouped))

  (define (budget-report s e accounts)
    (generate-budget-report (splits-by-account s e (map account-id accounts))))
  
  (define (splits-by-account s e acct-ids)
    (let* ([crossers (crossers (filter (make-date-filter s e) transactions) acct-ids)]
           [external-motion (apply append (map (lambda (transaction)
                                                 (external-splits transaction acct-ids))
                                               crossers))])
      (group-by-account external-motion)))
  
  (define (transactions-in-range s e)
    (filter (make-date-filter s e) transactions))
   


  (define (pair-up a b)
    (let ([ht (make-hash)])
      (for-each (match-lambda [(list k v) (hash-set! ht k (list v))]) a)
      (for-each (match-lambda [(list k v) (hash-set! ht k (cons v (hash-ref ht k (lambda () (list 0)))))]) b)
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
  
  (define (colonsep strlist)
    (apply string-append (cons (car strlist) (map (lambda (x) (string-append ":" x)) (cdr strlist)))))
  
  (define (digfmt n)
    (/ (* n 100) 100.0))
  
  

  
