(define current-interfaces (make-parameter (make-eq-hashtable)))

(define (lookup-interface name)
  (or (hashtable-ref (current-interfaces) name #f)
      (raise-extract-error
       "reference to unknown interface ~a" name)))

(define (extract-define-interface form)
  (match (cdr form)
    ((name ('compound-interface . interfaces))
     (let ((exports (append-map interface-exported-names interfaces)))
       (hashtable-set! (current-interfaces) name exports)
       `(interface (^ (name ,name)) ,(caddr form))))
    ((name ('export . exports))
     (hashtable-set! (current-interfaces) name exports)
     `(interface (^ (name ,name)) ,(caddr form)))
    (else
     (raise-extract-error "unmatched DEFINE-INTERFACE"))))

(define (r6rs-library-extractor dir)
  (lambda (form)
    (match (strip-non-forms (cdr form) 2)
      ((name ('export . exports) . clauses)
       `((structure (^ (name ,name))
                    (interface (export ,@exports))
                    ,@(r6rs-library-clauses->spedl dir clauses))))
      (else
       (raise-extract-error "unmatched LIBRARY")))))

(define (r6rs-library-clauses->spedl dir clauses)
  (loop continue ((with files '())
                  (with opens '())
                  (with items '())
                  (for clause (in-list clauses)))
    => (cons `(items ,@(cdr (scheme->spedl usual-spedl-extractors (reverse items))))
             (if (null? files)
                 '()
                 `((files ,@(reverse files)))))
    (match clause
      (('import imports ___)
       (continue (=> opens (cons (cdr clause) opens))))
      (('include-file filespec)
       (continue (=> files
                     (cons (pathname-join dir
                                          (filespec->pathname (cadr clause) "scm"))
                                 files))))
      ((x . y)
       (continue (=> items (cons clause items))))
      (_
       (continue)))))

(define (find-r6rs-libs dir)
  (define (library-file? pathname)
    (and (= 1 (length (file-types (pathname-file pathname))))
         (string=? (file-type (pathname-file pathname)) "sls")))
  (sort-list
   (directory-fold-tree
    (pathname-as-directory dir)
    (lambda (pathname libs)
      (if (library-file? pathname)
          (cons pathname libs)
          libs))
    (lambda (dirname libs)
      libs)
    '())
   pathname<?))

(define (filespec->pathname fspec ext)
  (cond ((or (symbol? fspec) (string? fspec))
         (make-pathname #f '() (make-file (maybe-symbol->string fspec) ext)))
        ((pair? fspec)
         (if (pair? (car fspec))
             (make-pathname #f
                            (map maybe-symbol->string (car fspec))
                            (make-file (maybe-symbol->string (cadr fspec)) ext))
             (make-pathname #f
                            (map maybe-symbol->string (drop-right fspec 1))
                            (make-file (maybe-symbol->string (last fspec)) ext))))
        (else
         (error 'filespec->pathname "cannot coerce to pathname" fspec))))

(define (config-language-extractors dir)
  `((define-structure . ,(extract-define-structure dir))
    (define-interface . ,extract-define-interface)))

(define (r6rs-toplevel-extractors dir)
  `((library . ,(r6rs-library-extractor dir))))

;; Local Variables:
;; scheme-indent-styles: ((match 1) foof-loop)
;; End:
