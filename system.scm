(define (systems->spedl . sysdefs)
  `(items
    ,@(append-map
       (lambda (sysdef)
         (cdr
          (call-with-file-and-dir sysdef
            (lambda (port)
              (let ((lib-docs (scheme->spedl
                               `((define-system . ,extract-define-system))
                               port)))
                (pre-post-order
                 lib-docs
                 `((spedl-files *preorder* .
                                ,(lambda (tag . filespecs)
                                   `(items ,@(snarf-files filespecs))))
                   (*fragment* . ,(lambda (tag . subs)
                                    `(items ,@subs)))
                   ,@universal-spedl-rules)))))))
       sysdefs)))

(define (extract-define-system form)
  (match (cdr form)
    ((list-rest name version clauses)
     `(system (@ (name ,name) (version ,version))
              ,@clauses))
    (else
     (raise-extract-error "unmatched DEFINE-SYSTEM"))))

(define (extract-define-structure form)
  (match (cdr form)
    ((list-rest name (cons 'export exports) clauses)
     `(structure (@ (name ,name))
                 (interface (export ,@exports))
                 ,@(filter (lambda (clause)
                             (memq (car clause) '(open files)))
                           clauses)))
    ((list-rest name interface clauses)
     `(structure (@ (name ,name))
                 (interface ,interface)
                 ,@(filter (lambda (clause)
                             (memq (car clause) '(open files)))
                           clauses)))
    (else
     (raise-extract-error "unmatched DEFINE-STRUCTURE"))))

(define (snarf-files filespecs)
  (append-map
   (lambda (filespec)
     (pre-post-order
      (call-with-input-file (filespec->path filespec)
        (lambda (port)
          (scheme->spedl `((define-structure . ,extract-define-structure)) port)))
      `((*fragment* *preorder* . ,(lambda (tag . subs) subs))
        ,@universal-spedl-rules)))
   filespecs))

(define (fspec-comp->path-comp fspec-comp)
  (cond ((symbol? fspec-comp) (symbol->string fspec-comp))
        (else fspec-comp)))

(define (filespec->path filespec)
  (append-extension
   (cond ((pair? filespec)
          (apply make-path (map fspec-comp->path-comp filespec)))
         ((symbol? filespec)
          (symbol->string filespec))
         (else filespec))
   "scm"))



;; arch-tag: bd627c26-88a9-49fa-ab9a-0f7d04d8d519
