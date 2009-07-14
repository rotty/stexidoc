;; -*- Mode: Scheme; scheme48-package: stexidoc.texi; -*-

(define universal-spedl->stexi-rules
  `((^ . ,(lambda (trigger . subs)
            `(% ,@subs)))
    (*fragment* *PREORDER* .
                ,(lambda (tag . spedls)
                   `(*fragment* ,@(append-map
                                   (lambda (spedl) (cdr (spedl->stexi spedl)))
                                   spedls))))
    (*DEFAULT* . ,list)
    (*TEXT* . ,(lambda (trigger text)
                 (cond ((symbol? text)
                        (symbol->string text))
                       (else text))))))
  
(define (spedl->stexi spedl)
  (pre-post-order
   spedl
   `((group
      ((items
        ((structure *MACRO* .
                    ,(lambda (tag attlist . subs)
                       (let ((name (car (assq-ref (cdr attlist) 'name)))
                             (bindings ((sxpath '(items *)) (cons tag subs)))
                             (docs (or (assq-ref subs 'documentation) '()))
                             (interface (assq-ref subs 'interface)))
                         `(structure* ,docs ,@(filter-items interface bindings)))))
         (structure* . ,process-structure*)
         (procedure . ,(make-def 'defun 'defunx))
         (arguments *PREORDER* . ,arguments->stexi)
         (variable . ,(make-def 'defvar 'defvarx))
         (syntax . ,(make-def 'defspec 'defspecx))
         ,@universal-spedl->stexi-rules)
        . ,(lambda (tag . procs) procs))
       (documentation *PREORDER* . ,(lambda (tag . docs) docs))
       ,@universal-spedl->stexi-rules)
      . ,process-group)
     ,@universal-spedl->stexi-rules)))


(define (process-group tag items docs)
  (cond ((not (null? items))
         `(*fragment*
           ,@((car items) (append
                           (append-map (lambda (proc) (proc #f)) (cdr items))
                           docs))))
        (else
         '(*fragment*))))

(define (process-structure* tag docs . items)
  (lambda (to-wrap)
    `((section "Overview")
      ,@to-wrap
      (section "Usage")
      ,@docs
      ,@(append-map cdr items))))

(define (arguments->stexi tag . args)
  `(arguments
    ,@(fold-right
       (lambda (arg args)
         (match arg
           (('rest-list arg)
            (append! (list "." (symbol->string arg) 
                           args)))
           (('optional . opt-args)
            `("[" ,@(map symbol->string opt-args) "]"
              ,@args))
           (else
            (cons (if (symbol? arg)
                      (symbol->string arg)
                      arg)
                  args))))
       '()
       args)))

(define (make-def type typex)
  (lambda (tag attlist . subs)
    (lambda (to-wrap)
      (if to-wrap
          `((,type ,attlist ,@to-wrap))
          `((,typex ,attlist))))))

(define (filter-items interface items)
  (let ((names (interface-exported-names interface)))
    (define (do-filter tag attlist . subs)
      (let ((name (car (assq-ref (cdr attlist) 'name))))
        (and (memq name names)
             `(,tag ,attlist ,@subs))))
    (pre-post-order
     items
     `((group
        ((items
          ((procedure *PREORDER* . ,do-filter)
           (variable *PREORDER* . ,do-filter)
           (syntax *PREORDER* . ,do-filter))
          . ,(lambda (tag . subs)
               `(items ,@(filter identity subs))))
         (*DEFAULT* *PREORDER* . ,list))
        . ,list)
       (documentation *PREORDER* . ,list)))))

;; arch-tag: be0af3e7-6a35-4b70-9550-6cdbe6a5781c