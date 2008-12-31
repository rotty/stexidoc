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
                 `((spedl-files
                    *PREORDER*
                    . ,(lambda (tag . filespecs)
                         (let ((dir (x->pathname sysdef)))
                           `(items ,@(snarf-files
                                      (config-language-extractors dir)
                                      (map (lambda (fspec)
                                             (pathname-attach-directory
                                              dir (filespec->pathname fspec)))
                                           filespecs))))))
                   (*fragment* . ,(lambda (tag . subs)
                                    `(items ,@subs)))
                   ,@universal-spedl-rules)))))))
       sysdefs)))

(define (extract-define-system form)
  (match (cdr form)
    ((list-rest name clauses)
     `(system (^ (name ,name))
              ,@clauses))
    (else
     (raise-extract-error "unmatched DEFINE-SYSTEM"))))

(define (extract-define-structure dir)
  (lambda (form)
    (match (cdr form)
      ((list-rest name (cons 'export exports) clauses)
       `(structure (^ (name ,name))
                   (interface (export ,@exports))
                   ,@(structure-clauses->spedl dir clauses)))
      ((list-rest name interface clauses)
       `(structure (^ (name ,name))
                   (interface (^ (name interface))
                     (export ,@(lookup-interface interface)))
                   ,@(structure-clauses->spedl dir clauses)))
      (else
       (raise-extract-error "unmatched DEFINE-STRUCTURE")))))

(define (structure-clauses->spedl dir clauses)
  (filter-map (lambda (clause)
                (case (car clause)
                  ((open) clause)
                  ((files)
                   (let ((fspec->path
                          (lambda (fspec)
                            (pathname-attach-directory dir (filespec->pathname fspec)))))
                     `(files ,@(map fspec->path (cdr clause)))))
                  (else #f)))
              clauses))

(define current-interfaces (make-parameter (make-table 'eq)))

(define (interface-exports interface)
  (cond ((symbol? interface)
         (lookup-interface interface))
        (else 
         (assq-ref interface 'export))))

(define (interface-exported-names interface)
  (let ((exports (interface-exports interface)))
    (let loop ((in exports) (out '()))
      (if (null? in)
          out
          (let ((elt (car in)))
            (cond ((and (pair? elt) (pair? (car elt)))
                   (loop (cdr in) (append (car elt) out)))
                  ((pair? elt)
                   (loop (cdr in) (cons (car elt) out)))
                  (else
                   (loop (cdr in) (cons elt out)))))))))

(define (lookup-interface name)
  (or (table-ref (current-interfaces) name)
      (raise-extract-error
       "reference to unknown interface ~a" name)))

(define (extract-define-interface form)
  (match (cdr form)
    ((list name (list-rest 'compound-interface interfaces))
     (let ((exports (append-map interface-exported-names interfaces)))
       (table-set! (current-interfaces) name exports)
       `(interface (^ (name ,name)) ,(caddr form))))
    ((list name (cons 'export exports))
     (table-set! (current-interfaces) name exports)
     `(interface (^ (name ,name)) ,(caddr form)))
    (else
     (raise-extract-error "unmatched DEFINE-INTERFACE"))))

(define (maybe-symbol->string x)
  (if (symbol? x) (symbol->string x) x))

(define (filespec->pathname fspec)
  (cond ((symbol? fspec)
         (make-pathname #f '() (make-file (symbol->string fspec) "scm")))
        ((pair? fspec)
         (if (pair? (car fspec))
             (make-pathname #f (map maybe-symbol->string (car fspec))
                            (make-file (maybe-symbol->string (cadr fspec)) "scm"))
             (make-pathname #f (map maybe-symbol->string (drop-right fspec 1))
                            (make-file (maybe-symbol->string (last fspec)) "scm"))))
        (else
         (error "cannot coerce to pathname" fspec))))

(define (pathname-attach-directory dir pathname)
  (make-pathname (pathname-origin dir)
                 (append (pathname-directory dir)
                         (pathname-directory pathname))
                 (pathname-file pathname)))

(define (config-language-extractors dir)
  `((define-structure . , (extract-define-structure dir))
    (define-interface . ,extract-define-interface)))

;; arch-tag: bd627c26-88a9-49fa-ab9a-0f7d04d8d519