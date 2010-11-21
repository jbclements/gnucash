(module shelly mzscheme
  
  (provide cd ls sa sasp cp pf p2ss sys2 sys2out sys2out* run-as-tester 
           onlydirs onlyfiles
           with-cd for-each-dir map-over-dirs for-each-file for-each-file/recursive
           okayokay
           no-falses
           file->lines)
  
  (require (lib "list.ss" "srfi" "1")
           (lib "process.ss")
           (lib "md5.ss")
           (lib "26.ss" "srfi"))
  
  (provide display-only) ; used to test commands before running them
  
  (define display-only (make-parameter #f))
  
  (define cd current-directory)
  (define ls directory-list)
  (define sa string-append)
  (define (sasp los) (apply string-append (map (cut sa <> " ") los)))
  (define cp copy-file)
  (define (p2ss los) (map path->string los))
  
  ;; evaluate a shell command
  (define sys2 (lambda a (let ([line (apply sa a)])(begin (display line) (printf "\n") (unless (display-only)(system line))))))
  
  (define (run-as-tester . args) 
    (let ([cd (path->string (current-directory))])
      (when (regexp-match "'" (apply string-append args))
        (error 'run-as-tester "arglist contains single quote:" args))
      (apply sys2 `("ssh submission-tester@localhost 'cd " ,cd " ; " ,@args "'"))))
  
  ;; evaluate a shell command (string style), return result as a list of strings
  (define sys2out (lambda a (let ([line (apply sa a)])
                              (begin (display line) (printf "\n")
                                     (sys2out/helper (process line))))))
  
  (define sys2out* (lambda args
                     (begin (printf "~v\n" args)
                            (sys2out/helper (apply process* args)))))
  
  (define pf (lambda (path)
               (sys2 "enscript -2rGh " (path->string path))))
  
  
  
  
  ;; take the ports that result from a call to process or process* and gather the output into a list of strings.
  (define (sys2out/helper list-of-ports)
    (let* ([result-port (list-ref list-of-ports 0)]
           [input-port (list-ref list-of-ports 1)]
           [error-port (list-ref list-of-ports 3)]
           [interact-proc (list-ref list-of-ports 4)])
      (close-output-port input-port)
      (let ([result-lines (let loop ()
                            (let ([r (read-line result-port)])
                              (if (eof-object? r)
                                  `()
                                  (cons r (loop)))))])
        (close-input-port result-port)
        (close-input-port error-port)
        (interact-proc 'wait)
        (if (not (= 0 (interact-proc 'exit-code)))
            (error 'sys2out "process caused an error.")
            result-lines))))
  
  (define (onlydirs l) (filter directory-exists? l))
  (define (onlyfiles l) (filter file-exists? l))
  
  (define-syntax (with-cd stx)
    (syntax-case stx ()
      [(_ dir code ...) #`(parameterize ([current-directory dir]) code ...)]
      [(_ any ...) (error "ffo")]))
  
  (define-syntax (map-over-dirs stx)
    (syntax-case stx ()
      [(_ dir-var . rest)
       #`(map
          (lambda (dir-var)
            (with-cd dir-var 
              . rest))
          (onlydirs (ls)))]))
  
  (define-syntax (for-each-dir stx)
    (syntax-case stx ()
      [(_ dir-var . rest)
       #`(for-each 
          (lambda (dir-var)
            (with-cd dir-var 
              . rest))
          (onlydirs (ls)))]))
  
  (define-syntax (for-each-file stx)
    (syntax-case stx ()
      [(_ file-var . rest)
       #`(for-each 
          (lambda (file-var)
            . rest)
          (onlyfiles (ls)))]))
  
  
  (define (for-each-file/recursive/proc thunk dirpath)
    (for-each thunk (onlyfiles (map (lambda (x) (build-path dirpath x)) (ls dirpath))))
    (for-each (lambda (newdir) 
                (for-each-file/recursive/proc thunk newdir))
              (onlydirs (map (lambda (x) (build-path dirpath x))(ls dirpath)))))
  
  (define-syntax (for-each-file/recursive stx)
    (syntax-case stx ()
      [(_ file-var . rest)
       #`(for-each-file/recursive/proc (lambda (file-var) . rest) (current-directory))]))
  
  (define-syntax (okayokay stx)
    (syntax-case stx ()
      [(_ . rest) #`(okayokay/proc (lambda () . rest))]))
  
  (define (okayokay/proc thunk)
    (with-handlers ([exn:fail?
                     (lambda (exn)
                       (fprintf (current-error-port)
                                "caught exception with error message:\n~a\ncontinuing...\n"
                                (exn-message exn)))])
      (thunk)))
  
  (define (no-falses l)
    (filter (lambda (x) (not (not x))) l))
  
  (define (file->lines path)
    (with-input-from-file path
      (lambda ()
        (let loop ()
          (let ([line (read-line (current-input-port) 'any)])
            (if (eof-object? line)
                null
                (cons line
                      (loop))))))))
        

)
