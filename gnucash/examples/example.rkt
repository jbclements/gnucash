#lang racket

;; replace these with pkg requires if you use this as a template somewhere else:
(require "../libs.rkt"
         "../typed-libs.rkt"
         "../parse.rkt"
         racket/runtime-path)

(define-runtime-path here-path ".")

;; ordinarily, the source file would be in some other directory...
(define gnucash-file-source (expand-user-path (build-path here-path "sample-gnucash-file")))
(define gnucash-zo-file (expand-user-path (build-path here-path "sample-gnucash.zo")))

;; this will take a very long time the first time you call it on a sizeable gnucash file.
;; my gnucash file is about 19 Meg after unzipping, and the translation to a .zo file takes
;; almost 3 minutes on my intel laptop.
(define gnucash-data 
  (time (gnucash-read gnucash-file-source gnucash-zo-file)))

;; can do they have expected type tags?
(define _1 (group-by first gnucash-data))

;(init-libs gnucash-data)
(define accounts (parsed->accounts gnucash-data))
(define transactions (parsed->transactions gnucash-data))

;; show all the account names
(printf "Account names: ~v\n" (map (curryr account-name-path accounts) accounts))

;; locate the id for the checking account
(define checking-account-id (account-id
                             (find-account
                              `("Root Account"
                                "Assets" "Current Assets" "Checking Account")
                              accounts)))

;; find all the transactions into or out of "Assets:Current Assets:Checking Account"
(define my-transactions (crossers transactions (list checking-account-id)))

;; display their dates and the net flow into or out of the checking account
(printf "Net Flows: ~v\n" 
        (map (lambda (t)
               (list (transaction-date t)
                     (net t (list checking-account-id) dollars)))
             my-transactions))

(define checking-accounts
  (find-accounts/prefix
   '("Root Account" "Assets" "Current Assets" "Checking Account")
   accounts))

(define accounts-with-splits
  (map (compose (curryr account-name-path accounts)
                (curryr id->account accounts)
                split-account
                second)
       (transaction-splits (first transactions))))

(module+ test
  
  (require rackunit
           srfi/19)
  ;; regression test:
  (check-equal? 
   (length (year->transactions 2007 transactions))
   7)

  ;; regression test:
  (check-equal?
   (take (apply append (map transaction-splits transactions))
         6)
   (list
    (list
     (make-time 'time-utc 0 1170144000)
     '(trn:split
       (split:id (@ (type "guid")) "9374fea23a266b1ee6162c1c6f02cb77")
       (split:reconciled-state "n")
       (split:value "100000/100")
       (split:quantity "100000/100")
       (split:account (@ (type "guid")) "ae3cb692a4101e744f4ff021896178a8")))
    (list
     (make-time 'time-utc 0 1170144000)
     '(trn:split
       (split:id (@ (type "guid")) "f59e16783fa01fd3cc60d9f4c06f082e")
       (split:reconciled-state "n")
       (split:value "-100000/100")
       (split:quantity "-100000/100")
       (split:account (@ (type "guid")) "679a80e5250a3d31670f64d8224d7a88")))
    (list
     (make-time 'time-utc 0 1172822400)
     '(trn:split
       (split:id (@ (type "guid")) "f2e57c66fc214622acf9428c06b8807c")
       (split:reconciled-state "n")
       (split:value "100000/100")
       (split:quantity "100000/100")
       (split:account (@ (type "guid")) "ae3cb692a4101e744f4ff021896178a8")))
    (list
     (make-time 'time-utc 0 1172822400)
     '(trn:split
       (split:id (@ (type "guid")) "ce2ca00fad0ad55a5ba05dba6da8fec1")
       (split:reconciled-state "n")
       (split:value "-100000/100")
       (split:quantity "-100000/100")
       (split:account (@ (type "guid")) "679a80e5250a3d31670f64d8224d7a88")))
    (list
     (make-time 'time-utc 0 1172995200)
     '(trn:split
       (split:id (@ (type "guid")) "78d96a5fcc8ea3eb92a8679556d75b48")
       (split:reconciled-state "n")
       (split:value "20000/100")
       (split:quantity "20000/100")
       (split:account (@ (type "guid")) "5ae3bbafcf214a8b482ad01e13b7b922")))
    (list
     (make-time 'time-utc 0 1172995200)
     '(trn:split
       (split:id (@ (type "guid")) "cc9f79157b32d8d80a6932992b9302ff")
       (split:reconciled-state "n")
       (split:value "-20000/100")
       (split:quantity "-20000/100")
       (split:account (@ (type "guid")) "ae3cb692a4101e744f4ff021896178a8")))))

  ;; regression:
  (check-equal?
   (list->set (group-by-account (transaction-splits (first transactions))))
   (set
    (list
     "679a80e5250a3d31670f64d8224d7a88"
     (list
      (list
       (make-time 'time-utc 0 1170144000)
       '(trn:split
         (split:id (@ (type "guid")) "f59e16783fa01fd3cc60d9f4c06f082e")
         (split:reconciled-state "n")
         (split:value "-100000/100")
         (split:quantity "-100000/100")
         (split:account (@ (type "guid")) "679a80e5250a3d31670f64d8224d7a88")))))
    (list
     "ae3cb692a4101e744f4ff021896178a8"
     (list
      (list
       (make-time 'time-utc 0 1170144000)
       '(trn:split
         (split:id (@ (type "guid")) "9374fea23a266b1ee6162c1c6f02cb77")
         (split:reconciled-state "n")
         (split:value "100000/100")
         (split:quantity "100000/100")
         (split:account (@ (type "guid")) "ae3cb692a4101e744f4ff021896178a8"))))))))

