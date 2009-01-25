(define (systems->spedl . sysdefs)
  `(items
    ,@(append-map
       (lambda (sysdef)
         (let ((sys-dir (pathname-with-file (x->pathname sysdef) #f)))
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
                           `(items ,@(files->spedl
                                      (config-language-extractors sys-dir)
                                      (map
                                       (lambda (fspec)
                                         (pathname-join sys-dir
                                                        (filespec->pathname fspec)))
                                       filespecs)))))
                     (r6rs-libs
                      *PREORDER*
                      . ,(lambda (tag . dirs)
                           `(items
                             ,@(files->spedl
                                (r6rs-toplevel-extractors (pathname-container sys-dir))
                                (append-map
                                 (lambda (dir)
                                   (find-r6rs-libs (pathname-join sys-dir dir)))
                                 dirs)))))
                     (*fragment* . ,(lambda (tag . subs)
                                      `(items ,@subs)))
                     ,@universal-spedl-rules))))))))
       sysdefs)))

(define (extract-define-system form)
  (match (cdr form)
    ((list-rest name clauses)
     `((system (^ (name ,name))
               ,@clauses)))
    (else
     (raise-extract-error "unmatched DEFINE-SYSTEM"))))

(define (extract-define-structure dir)
  (lambda (form)
    (match (cdr form)
      ((list-rest name (cons 'export exports) clauses)
       `((structure (^ (name ,name))
                    (interface (export ,@exports))
                    ,@(structure-clauses->spedl dir clauses))))
      ((list-rest name interface clauses)
       `((structure (^ (name ,name))
                    (interface (^ (name interface))
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
                            (pathname-join dir (filespec->pathname fspec)))))
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

(define (r6rs-library-extractor dir)
  (lambda (form)
    (match (strip-non-forms (cdr form) 2)
      ((list-rest name (cons 'export exports) clauses)
       `((structure (^ (name ,name))
                    (interface (export ,@exports))
                    ,@(r6rs-library-clauses->spedl dir clauses))))
      (else
       (raise-extract-error "unmatched LIBRARY")))))

(define (r6rs-library-clauses->spedl dir clauses)
  (let loop ((files '()) (opens '()) (items '()) (clauses clauses))
    (if (null? clauses)
        (cons `(items ,@(cdr (scheme->spedl usual-spedl-extractors (reverse items))))
                (if (null? files)
                    '()
                    `((files ,@(reverse files)))))
        (let ((clause (car clauses)))
          (if (pair? clause)
              (case (car clause)
                ((import)
                 (loop files (cons (cdr clause) opens) items (cdr clauses)))
                ((include-file)
                 (loop (cons (pathname-join dir (filespec->pathname (cadr clause)))
                             files)
                       opens
                       items
                       (cdr clauses)))
                (else
                 (loop files opens (cons clause items) (cdr clauses))))
              (loop files opens items (cdr clauses)))))))

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

(define (config-language-extractors dir)
  `((define-structure . ,(extract-define-structure dir))
    (define-interface . ,extract-define-interface)))

(define (r6rs-toplevel-extractors dir)
  `((library . ,(r6rs-library-extractor dir))))

;; arch-tag: bd627c26-88a9-49fa-ab9a-0f7d04d8d519
