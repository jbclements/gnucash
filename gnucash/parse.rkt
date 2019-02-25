#lang racket/base

(require sxml
         racket/format
         racket/system
         racket/match
         racket/contract
         racket/function)

;(require (for-syntax scheme))

(provide (contract-out
          [gnucash-read (-> path-string? path-string? (listof any/c))]))

;; WARNING: assumes a unix-y path convention.  Fix this if you like.

;; compile a gnucash file into a .zo file.  On my file, this makes a 
;; 3-minute process into a ten-second one.
;;
;; EFFECT: stomps certain files in /tmp, leaves junk behind.
;;
(define (cache-as-zo input-file output-file)
  (let ([str (~a "cp "(path->string input-file)" /tmp/gnucash-expanded.gz")])
    (printf "~a\n" str)
    (system str))
  (when (file-exists? "/tmp/gnucash-expanded")
    (delete-file "/tmp/gnucash-expanded"))
  (let ([str (~a "gunzip /tmp/gnucash-expanded.gz")])
    (printf "~a\n" str)
    (system str))
  (call-with-input-file "/tmp/gnucash-expanded"
    (lambda (port)
      (define parsed
        (ssax:xml->sxml port gnucash-namespace-abbreviations))
      (printf "done parsing.\n")
      (with-output-to-file output-file
        (lambda ()
          (write (compile #`(quote #,parsed))))
        #:exists
        'truncate)
      (printf "done compiling and writing .zo file\n"))))

;; the problem with namespaces is that they generate sxml
;; listing taken from
;; https://github.com/Gnucash/gnucash/blob/maint/libgnucash/doc/xml/gnucash-v2.rnc
(define gnucash-namespace-abbreviations
  '((gnc . "http://www.gnucash.org/XML/gnc")
    (act . "http://www.gnucash.org/XML/act")
    (book . "http://www.gnucash.org/XML/book")
    (cd . "http://www.gnucash.org/XML/cd")
    (cmdty . "http://www.gnucash.org/XML/cmdty")
    (price . "http://www.gnucash.org/XML/price")
    (slot . "http://www.gnucash.org/XML/slot")
    (split . "http://www.gnucash.org/XML/split")
    (sx . "http://www.gnucash.org/XML/sx")
    (trn . "http://www.gnucash.org/XML/trn")
    (ts . "http://www.gnucash.org/XML/ts")
    (fs . "http://www.gnucash.org/XML/fs")
    (bgt . "http://www.gnucash.org/XML/bgt")
    (recurrence . "http://www.gnucash.org/XML/recurrence")
    (lot . "http://www.gnucash.org/XML/lot")
    (addr . "http://www.gnucash.org/XML/addr")
    (owner . "http://www.gnucash.org/XML/owner")
    (billterm . "http://www.gnucash.org/XML/billterm")
    (bt-days . "http://www.gnucash.org/XML/bt-days")
    (bt-prox . "http://www.gnucash.org/XML/bt-prox")
    (cust . "http://www.gnucash.org/XML/cust")
    (employee . "http://www.gnucash.org/XML/employee")
    (entry . "http://www.gnucash.org/XML/entry")
    (invoice . "http://www.gnucash.org/XML/invoice")
    (job . "http://www.gnucash.org/XML/job")
    (order . "http://www.gnucash.org/XML/order")
    (taxtable . "http://www.gnucash.org/XML/taxtable")
    (tte . "http://www.gnucash.org/XML/tte")
    (vendor . "http://www.gnucash.org/XML/vendor")))

;; read gnucash data from a .zo file produced by cache-as-zo
(define (read-from-zo zo-file)
  (eval (parameterize ([read-accept-compiled #t])
          (call-with-input-file zo-file read))))

;; strip top level XML to get a list of gnucash "things"
(define (strip-top-level-goo xml-elt)
  (match xml-elt
    [`(*TOP* ,top-attribs ,pi-node
             (gnc-v2 (gnc:count-data (@ (cd:type "book")) "1")
                     (gnc:book (@ (version "2.0.0")) . ,content)))
     content]))

;; use this to read a gnucash file. Returns a list of sxml elements
(define (gnucash-read gnucash-file gnucash-cache-file)
  ;; refresh cache if necessary
  (when (or (not (file-exists? gnucash-cache-file))
            (= (file-size gnucash-cache-file) 0)
            (< (file-or-directory-modify-seconds gnucash-cache-file)
               (file-or-directory-modify-seconds gnucash-file)))
    (cache-as-zo gnucash-file gnucash-cache-file))
  (strip-top-level-goo (read-from-zo gnucash-cache-file)))


