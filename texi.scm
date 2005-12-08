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
     (procedure . ,(lambda (tag attlist)
                     `(defun ,attlist)))
     (variable . ,(lambda (tag attlist)
                    `(defvar ,attlist)))
     (syntax . ,(lambda (tag attlist)
                  `(defspec ,attlist)))
     ,@universal-spedl->stexi-rules)))

(define (process-group . spedl)
  (pre-post-order
   spedl
   `((group . ,(lambda (tag proc . procs)
                 (cond ((procedure? proc)
                        (proc (append-map (lambda (proc) (proc #f)) procs)))
                       (else proc))))
     (structure *macro* .
                ,(lambda (tag attlist . subs)
                   (let ((name (car (assq-ref (cdr attlist) 'name)))
                         (bindings (assq-ref subs 'bindings))
                         (interface (assq-ref subs 'interface)))
                     `(structure* ,@(filter-bindings interface bindings)))))
     (structure* . ,(lambda (tag . bindings)
                      (lambda (to-wrap)
                        `((section "Overview")
                          ,@to-wrap
                          (section "Usage")
                          ,@bindings))))
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
     (documentation *preorder* . ,(lambda (tag . subs)
                                    (lambda (to-wrap)
                                      subs)))
     ,@universal-spedl->stexi-rules)))


(define (make-deffn category)
  (lambda (tag attlist . subs)
    (lambda (to-wrap)
      (if to-wrap
          `(deffn (% (category ,category) ,@(cdr attlist))
             ,@to-wrap)
          `((deffnx (% (category ,category) ,@(cdr attlist))))))))

(define (make-def type typex)
  (lambda (tag attlist . subs)
    (lambda (to-wrap)
      (if to-wrap
          `(,type ,attlist ,@to-wrap)
          `((,typex ,attlist))))))

(define (interface-exported-names interface)
  (let loop ((in (cdar interface)) (out '()))
    (if (null? in)
        out
        (let ((elt (car in)))
          (cond ((and (pair? elt) (pair? (car elt)))
                 (loop (cdr in) (append (car elt) out)))
                ((pair? elt)
                 (loop (cdr in) (cons (car elt) out)))
                (else
                 (loop (cdr in) (cons elt out))))))))

(define (filter-bindings interface bindings)
  (let ((names (interface-exported-names interface)))
    (define (do-filter tag attlist . subs)
      (let ((name (car (assq-ref (cdr attlist) 'name))))
        (and (memq name names)
             `(,tag ,attlist ,@subs))))
    (pre-post-order
     bindings
     `((group . ,(lambda (tag . subs)
                   `(group ,@(filter identity subs))))
       (procedure *preorder* . ,do-filter)
       (variable *preorder* . ,do-filter)
       (syntax *preorder* . ,do-filter)
       (documentation *preorder* . ,(lambda args args))))))

;; arch-tag: be0af3e7-6a35-4b70-9550-6cdbe6a5781c
