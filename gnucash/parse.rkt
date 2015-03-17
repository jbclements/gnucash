#lang racket/base

(require sxml
         racket/format
         racket/system
         racket/match)

;(require (for-syntax scheme))

(provide (all-defined-out))

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
  (with-input-from-file "/tmp/gnucash-expanded"
    (lambda ()
      (let* ([parsed (ssax:xml->sxml (current-input-port) '())])
        (with-output-to-file output-file
          (lambda ()
            (write (compile #`(quote #,parsed))))
          #:exists
          'truncate)))))

;; read gnucash data from a .zo file produced by cache-as-zo
(define (read-from-zo zo-file)
  (eval (parameterize ([read-accept-compiled #t])
          (call-with-input-file zo-file read))))

;; strip top level XML to get a list of gnucash "things"
(define (strip-top-level-goo xml-elt)
  (match xml-elt
    [`(*TOP* ,top-attribs (gnc-v2 ,count-data (,book-tag (@ . ,book-attribs) . ,content)))
     content]))

;; use this to read a gnucash file
(define (gnucash-read gnucash-file gnucash-cache-file)
  ;; refresh cache if necessary
  (when (or (not (file-exists? gnucash-cache-file))
            (< (file-or-directory-modify-seconds gnucash-cache-file)
               (file-or-directory-modify-seconds gnucash-file)))
    (cache-as-zo gnucash-file gnucash-cache-file))
  (strip-top-level-goo (read-from-zo gnucash-cache-file)))


