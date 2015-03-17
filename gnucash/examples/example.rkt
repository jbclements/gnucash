#lang racket

;; Astonishingly, all of this code still works, eight years later!

;; replace these with PLaneT requires if you use this as a template somewhere else:
(require "../libs.rkt"
         "../typed-libs.rkt"
         "../parse.rkt"
         racket/runtime-path)

(define-runtime-path here-path ".")

;; ordinarily, the source file would be in some other directory...
(define gnucash-file-source (expand-user-path (build-path here-path "sample-gnucash-file")))
(define gnucash-zo-file (expand-user-path (build-path here-path "sample-gnucash.zo")))

(define gnucash-data 
  (time (gnucash-read gnucash-file-source gnucash-zo-file)))

;; this will take a very long time the first time you call it on a sizeable gnucash file.
;; my gnucash file is about 19 Meg after unzipping, and the translation to a .zo file takes
;; almost 3 minutes on my intel laptop.
(init-libs gnucash-data)

;; show all the account names
(printf "Account names: ~v\n" (map account-name-path accounts))

;; locate the id for the checking account
(define checking-account-id (account-id
                             (find-account
                              `("Root Account"
                                "Assets" "Current Assets" "Checking Account"))))

;; find all the transactions into or out of "Assets:Current Assets:Checking Account"
(define my-transactions (crossers transactions (list checking-account-id)))

;; display their dates and the net flow into or out of the checking account
(printf "Net Flows: ~v\n" 
        (map (lambda (t)
               (list (transaction-date t)
                     (net t (list checking-account-id) dollars)))
             my-transactions))

