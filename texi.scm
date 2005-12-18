(define universal-spedl->stexi-rules
  `((@ . ,(lambda (trigger . subs)
            `(% ,@subs)))
    (*default* . ,(lambda args args))
    (*text* . ,(lambda (trigger text)
                 (cond ((symbol? text)
                        (symbol->string text))
                       (else text))))))
  
(define (spedl->stexi spedl)
  (pre-post-order
   spedl
   `((group *preorder* . ,process-group)
     ,@universal-spedl->stexi-rules)))

(define (process-group . spedl)
  (pre-post-order
   spedl
   `((group
      ((items
        ((structure *macro* .
                     ,(lambda (tag attlist . subs)
                        (let ((name (car (assq-ref (cdr attlist) 'name)))
                              (bindings (assq-ref subs 'items))
                              (interface (assq-ref subs 'interface)))
                          `(structure* ,@(filter-items interface bindings)))))
         (structure* . ,(lambda (tag . items)
                          (lambda (to-wrap)
                            `((section "Overview")
                              ,@to-wrap
                              (section "Usage")
                              ,@(append-map (lambda (proc)
                                              (cond ((procedure? proc) (proc #f))
                                                    (else proc)))
                                            items)))))
         (procedure . ,(make-def 'defun 'defunx))
         (arguments *preorder* .
                    ,(lambda (tag . args)
                       `(arguments
                         ,@(fold-right
                            (lambda (arg args)
                              (match arg
                                ((list 'rest-list arg)
                                 (append! (list "." (symbol->string arg) 
                                                args)))
                                ((list-rest 'optional opt-args)
                                 `("[" ,@(map symbol->string opt-args) "]"
                                   ,@args))
                                (else (cons (symbol->string arg) args))))
                            '()
                            args))))
         (variable . ,(make-def 'defvar 'defvarx))
         (syntax . ,(make-def 'defspec 'defspecx))
         ,@universal-spedl->stexi-rules)
        . ,(lambda (tag . procs)
             (lambda (to-wrap)
               (append-map (lambda (proc) (proc to-wrap)) procs))))
       (documentation *preorder* . ,(lambda (tag . subs)
                                      (lambda (to-wrap) subs)))
       ,@universal-spedl->stexi-rules)
      . ,(lambda (tag proc . procs)
           (cond ((procedure? proc)
                  (proc (append-map (lambda (proc) (proc #f)) procs)))
                 (else proc)))))))


(define (make-deffn category)
  (lambda (tag attlist . subs)
    (lambda (to-wrap)
      (if to-wrap
          `((deffn (% (category ,category) ,@(cdr attlist)) ,@to-wrap))
          `((deffnx (% (category ,category) ,@(cdr attlist))))))))

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
          ((procedure *preorder* . ,do-filter)
           (variable *preorder* . ,do-filter)
           (syntax *preorder* . ,do-filter))
          . ,(lambda (tag . subs)
               `(items ,@(filter identity subs))))
         (*default* *preorder* . ,list))
        . ,list)
       (documentation *preorder* . ,list)))))

;; arch-tag: be0af3e7-6a35-4b70-9550-6cdbe6a5781c
