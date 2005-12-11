(define nl (string #\newline))

(define universal-spedl-rules
  `((*text* . ,(lambda (tag text) text))
    (*default* . ,(lambda args args))))


(define scheme->spedl
  (opt-lambda (extractors (port (current-input-port)))
    (let ((comments '())
          (extracted '()))
      (define (maybe-emit-documentation!)
        (and (not (null? extracted))
             (receive (override? texi-fragment) (schmooz (reverse comments)
                                                         (car extracted))
               (let ((docs `(group (items ,@(if override? '() (reverse extracted)))
                                   (documentation
                                    ,@(cdr (texi-fragment->stexi texi-fragment))))))
                 (set! extracted '())
                 (set! comments '())
                 docs))))
      (pre-post-order
       (read-scheme-code port)
       `((form *preorder* .
               ,(lambda (tag form)
                  (guard
                   (c
                    ((extract-error? c)
                     (format #t "while processing ~s: ~s~%" form
                             (condition-message c))
                     (set! extracted '())
                     (set! comments '())))
                   (let ((result (cond
                                  ((and (pair? form)
                                        (assq (car form) extractors)) =>
                                        (lambda (x) ((cdr x) form)))
                                  (else #f))))
                     (if result
                         (set! extracted (cons result extracted)))
                     #f))))
         (comment *preorder* .
                  ,(lambda (tag comment)
                     (let ((docs (maybe-emit-documentation!)))
                       (set! comments (cons comment comments))
                       docs)))
         (*fragment* . ,(lambda (tag . subs)
                          `(*fragment*
                            ,@(filter (lambda (x) x)
                                      (cons (maybe-emit-documentation!) subs)))))
         ,@universal-spedl-rules)))))


(define-condition-type &extract-error &error
  extract-error?)

(define (raise-extract-error msg . args)
  (raise (condition (&extract-error)
                    (&message (message (apply format #f msg args))))))

(define (argspec->str spec)
  (cond ((symbol? spec) (symbol->string spec))
        ((and (pair? spec) (eq? (car spec) 'rest-list))
         (symbol->string (cadr spec)))
        (else
         (error "invalid argument spec" spec))))

(define (arglist->str-vector lst)
  (let ((n (length lst)))
    (let loop ((vec (make-vector n)) (i 0) (lst lst))
      (cond ((null? lst)    vec)
            ((pair? lst)
             (vector-set! vec i (argspec->str (car lst)))
             (loop vec (+ i 1) (cdr lst)))
            (else
             (vector-set! vec i (argspec->str lst))
             vec)))))

(define (expand-arg-macros matches line code)
  (let ((args (arglist->str-vector (cdar ((sxpath '(// @ arguments)) code))))
        (name (cadar ((sxpath '(// @ name)) code))))
    (let loop ((pos 0) (matches matches) (results '()))
      (if (null? matches)
          (apply string-append
                 (append results
                         (list (substring line pos (string-length line)))))
          (let* ((start (caar matches))
                 (end (cdar matches))
                 (n (string->number (substring line (+ start 1) end))))
            (loop end
                  (cdr matches)
                  (append results
                          (list
                           (substring line pos start)
                           "@var{"
                           (cond ((= 0 n) (symbol->string name))
                                 ((<= n (vector-length args))
                                  (vector-ref args (- n 1)))
                                 (else
                                  (raise-extract-error
                                   "no argument with number ~a" n)))
                           "} "))))))))

(define (match* pat str)
  (let loop ((pos 0) (matches '()))
    (let ((match (and (< pos (string-length str))
                      (pregexp-match-positions pat str pos))))
      (if match
          (loop (cdar match) (cons (car match) matches))
          (and (not (null? matches)) (reverse matches))))))

(define schmooz
  (let ((starts `((verbatim . ,(pregexp "^;*\\s*@def"))
                  (schmooz . ,(pregexp "^;*@"))))
        (lead-junk (pregexp "^(;*@\\s*|;+\\s*|;*\\s+)")))
    (lambda (comments code)
      ;; Find out what kind of markup started the comment
      (receive (comments mode)
          (let loop ((comments comments))
            (if (null? comments)
                (values '() 'schmooz)
                (let next-start ((starts starts))
                  (if (null? starts)
                      (loop (cdr comments))
                      (if (pregexp-match (cdar starts) (car comments))
                          (values comments (caar starts))
                          (next-start (cdr starts)))))))
        (case mode
          ((verbatim) (values #t (string-join (map (lambda (line)
                                                     (string-trim line #\;))
                                                   comments) nl)))
          ((schmooz)
           (let loop ((comments comments)
                      (result '()))
             (if (null? comments)
                 (values #f (string-join (reverse result) nl))
                 (let* ((comment (car comments))
                        (line (cond ((match* lead-junk comment)
                                     => (lambda (matches)
                                          (substring comment
                                                     (cdar matches)
                                                     (string-length comment))))
                                    (else comment))))
                   (cond ((match* "@[0-9]+" line)
                          => (lambda (matches)
                               (loop (cdr comments)
                                     (cons (expand-arg-macros matches line code)
                                           result))))
                         (else
                          (loop (cdr comments)
                                (cons line result)))))))))))))

(define (extract-define form)
  (match (cdr form)
    ((cons (cons name args) body)
     `(procedure (@ (name ,name) (arguments ,@(args->proper-list args)))))
    ((list name (list-rest 'lambda args body))
     `(procedure (@ (name ,name) (arguments ,@(args->proper-list args)))))
    ((list name expr)
     `(variable (@ (name ,name))))
    (else
     (raise-extract-error "unmatched define"))))

(define (extract-define-syntax form)
  (match (cdr form)
    ((list name expr)
     `(syntax (@ (name ,name))))
    (else
     (raise-extract-error "unmatched define-syntax"))))

(define (extract-define/optional-args form)
  (match (cadr form)
    ((list-rest name args)
     `(procedure
       (@ (name ,name)
          (arguments ,@(fold-right
                        (lambda (arg rest)
                          (cond ((and (pair? arg) (eq? (car arg) 'optional))
                                 (cons `(optional ,@(map car (cdr arg)))
                                       rest))
                                (else (cons arg rest))))
                        '()
                        args)))))
    (else
     (raise-extract-error "unmatched define/optional-args"))))

(define usual-spedl-extractors
  `((define . ,extract-define)
    (define-syntax . ,extract-define-syntax)
    (define/optional-args . ,extract-define/optional-args)))

(define (args->proper-list args)
  (let loop ((result '()) (lst args))
      (cond ((null? lst)  (reverse! result))
            ((pair? lst)  (loop (cons (car lst) result) (cdr lst)))
            (else         (loop (cons `(rest-list ,lst) result) '())))))


;; arch-tag: 6d052554-c085-4fd8-96c3-708f8bc3bb19
