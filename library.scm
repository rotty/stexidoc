(define (library->spedl library-dir)
  (call-with-file-and-dir (make-path library-dir "packages.scm")
    (lambda (port)
      (let ((lib-docs (scheme->spedl usual-spedl-extractors port)))
        (pre-post-order
         lib-docs
         `((group . ,(lambda (tag . subs)
                       `(group ,@(receive (code doc) (collect-code+doc subs)
                                   `(,@code
                                     (documentation ,@doc))))))
           (structure *preorder* . ,expand-structure)
           (documentation *preorder* . ,(lambda (tag . subs)
                                          (lambda ()
                                            (values #f subs))))
           (*fragment* . ,(lambda (tag . subs)
                            `(library ,@subs)))))))))

(define (expand-structure . args)
  (pre-post-order
   args
   `((structure . ,(lambda (tag . subs)
                     (lambda ()
                       (receive (code doc) (collect-code+doc subs)
                         (values `((structure ,@code)) doc)))))
     (files *preorder* .
            ,(lambda (tag . filespecs)
               (lambda ()
                 (receive (bindings doc) (snarf-files filespecs)
                   ;;(format #t "snarfed: ~s ~s\n" bindings doc)
                   (values `((bindings ,@bindings)) doc)))))
     (*text* . ,(lambda (tag text) text))
     (*default* . ,(lambda args args)))))

(define (snarf-files filespecs)
  (values
   (append-map
    (lambda (filespec)
      (pre-post-order
       (call-with-input-file (filespec->path filespec)
         (lambda (port)
          (scheme->spedl usual-spedl-extractors port)))
       `((*fragment* *preorder* . ,(lambda (tag . subs) subs))
         (*text* . ,(lambda (tag text) text))
         (*default* . ,(lambda args args)))))
    filespecs)
   #f))


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
