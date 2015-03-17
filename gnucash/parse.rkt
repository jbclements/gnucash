#lang racket
  
(require (only-in (planet clements/sxml2) ssax:xml->sxml))

(provide (all-defined-out))
  
  ;; WARNING: assumes a unix-y path convention.  Fix this if you like.
  
  ;; compile a gnucash file into a .zo file.  On my file, this makes a 
  ;; 3-minute process into a ten-second one.
  ;;
  ;; EFFECT: stomps certain files in /tmp, leaves junk behind.
  ;;
  (define (cache-as-zo input-file output-file)
    (sys2 "cp "(path->string input-file)" /tmp/gnucash-expanded.gz")
    (when (file-exists? "/tmp/gnucash-expanded")
      (delete-file "/tmp/gnucash-expanded"))
    (sys2 "gunzip /tmp/gnucash-expanded.gz")
    (with-input-from-file "/tmp/gnucash-expanded"
      (lambda ()
        (let* ([parsed (ssax:xml->sxml (current-input-port) '())])
          (with-output-to-file output-file
            (lambda ()
              (parameterize ([current-namespace (make-base-namespace)])
              (write (compile `',parsed))))
            #:exists
            'truncate)))))
  
  ;; read gnucash data from a .zo file produced by cache-as-zo
  (define (read-from-zo zo-file)
    (parameterize ([current-namespace (make-base-namespace)])
    (eval (parameterize ([read-accept-compiled #t])
            (file->value zo-file)))))
  
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
  
  
  ;; evaluate a shell command
  (define (sys2 . a) 
    (define line (apply string-append a))
    (printf "~s\n" line)
    (system line))
 


