(define (systems->spedl . sysdefs)
  `(items
    ,@(append-map
       (lambda (sysdef)
         (cdr
          (call-with-input-file (x->namestring sysdef)
            (lambda (port)
              (let ((lib-docs (scheme->spedl
                               `((define-system . ,extract-define-system))
                               port)))
                (pre-post-order
                 lib-docs
                 (lib-doc-rules
                  (pathname-with-file (x->pathname sysdef) #f))))))))
       sysdefs)))

(define (lib-doc-rules sys-dir)
  `((spedl-files
     *PREORDER*
     . ,(lambda (tag . filespecs)
          `(items ,@(files->spedl
                     (config-language-extractors sys-dir)
                     (map
                      (lambda (fspec)
                        (pathname-join sys-dir
                                       (filespec->pathname fspec "scm")))
                      filespecs)))))
    (r6rs-libraries
     *PREORDER*
     . ,(lambda (tag . specs)
          `(items
            ,@(append-map
               (lambda (spec)
                 (let ((pathname
                        (pathname-join sys-dir
                                       (filespec->pathname (if (pair? spec)
                                                               (car spec)
                                                               spec)
                                                           #f))))
                   (if (file-directory? pathname)
                       (files->spedl
                        (r6rs-toplevel-extractors pathname)
                        (find-r6rs-libs pathname))
                       (files->spedl
                        (r6rs-toplevel-extractors
                         (pathname-with-file pathname #f))
                        (list pathname)))))
               specs))))
    (*fragment* . ,(lambda (tag . subs)
                     `(items ,@subs)))
    ,@universal-spedl-rules))

(define (extract-define-system form)
  (match (cdr form)
    ((name . clauses)
     `((system (^ (name ,name))
               ,@clauses)))
    (else
     (raise-extract-error "unmatched DEFINE-SYSTEM"))))

(define (extract-define-structure dir)
  (lambda (form)
    (match (cdr form)
      ((name ('export . exports) . clauses)
       `((structure (^ (name ,name))
                    (interface (export ,@exports))
                    ,@(structure-clauses->spedl dir clauses))))
      ((name interface . clauses)
       `((structure (^ (name ,name))
                    (interface (^ (name ,interface))
                      (export ,@(lookup-interface interface)))
                    ,@(structure-clauses->spedl dir clauses))))
      (else
       (raise-extract-error "unmatched DEFINE-STRUCTURE")))))

(define (structure-clauses->spedl dir clauses)
  (filter-map (lambda (clause)
                (case (car clause)
                  ((open) clause)
                  ((files)
                   (let ((fspec->path
                          (lambda (fspec)
                            (pathname-join dir (filespec->pathname fspec "scm")))))
                     `(files ,@(map fspec->path (cdr clause)))))
                  (else #f)))
              clauses))

(define current-interfaces (make-parameter (make-eq-hashtable)))

(define (interface-exports interface)
  (cond ((symbol? interface)
         (lookup-interface interface))
        (else 
         (assq-ref interface 'export))))

(define (interface-exported-names interface)
  (let ((exports (interface-exports interface)))
    (loop continue ((for elt (in-list exports))
                    (with out '()))
      => out
      (cond ((and (pair? elt) (pair? (car elt)))
             (continue (=> out (append (car elt) out))))
            ((pair? elt)
             (continue (=> out (cons (car elt) out))))
            (else
             (continue (=> out (cons elt out))))))))

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
