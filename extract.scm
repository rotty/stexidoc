;; -*- Mode: Scheme; scheme48-package: stexidoc.extract; -*-

(define nl (string #\newline))

(define universal-spedl-rules
  `((*text* . ,(lambda (tag text) text))
    (*default* . ,(lambda args args))))

(define (scheme->spedl extractors port)
  (let loop ((comments '()) (extracted '()) (spedls '()) (code (read-scheme-code port)))
    (define (generate)
      (schmooz (reverse comments) (reverse extracted)))
    (cond ((and (null? code) (null? extracted) (null? comments))
           `(*fragment* ,@(reverse spedls)))
          ((null? code)
           (loop '() '() (append (generate) spedls) code))
          (else
           (case (caar code)
             ((form)
              (let ((form (cadar code)))
                (guard
                    (c
                     ((extract-error? c)
                      (format #t "while processing ~s: ~s~%" form
                              (condition-message c))
                      (loop '() '() spedls (cdr code))))
                  (let ((result (cond
                                 ((and (pair? form)
                                       (assq (car form) extractors)) =>
                                       (lambda (x) ((cdr x) form)))
                                 (else #f))))
                    (if result
                        (loop comments (cons result extracted) spedls (cdr code))
                        (loop comments extracted spedls (cdr code)))))))
             ((comment)
              (let ((comment (cadar code)))
                (cond
                 ((not (null? extracted))
                  (loop '() '() (append (generate) spedls) code))
                 (else
                  (loop (cons comment comments) extracted spedls (cdr code))))))
             (else
              (error "unexpected READ-SCHEME-CODE output" (car code))))))))

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

(define (expand-arg-macros matches line args code)
  (let ((name (cadar ((sxpath '(// @ name)) code))))
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

(define (arg-extended-items items args)
  `(items
    ,@(cond (args
             (map (lambda (item)
                    (match item
                      ((list-rest name (list-rest '@ attributes) clauses)
                       `(,name
                         (@ ,@attributes
                            (arguments ,@(map string->symbol (vector->list args))))
                         ,@clauses))
                      (else item)))
                  items))
            (else items))))

(define (schmooz-trim line)
  (let ((uncommented (string-trim line #\;)))
    (cond
     ((and (string-prefix? "@" uncommented)
           (or (= (string-length uncommented) 1) ;; lone "@"
               (string-skip uncommented char-set:whitespace))) ;; "@" followed by space
      (substring/shared uncommented 1 (string-length uncommented)))
     (else
      uncommented))))

(define schmooz
  (let ((starts `((verbatim . ,(pregexp "^;*\\s*@(def|(sub)*(section|heading))"))
                  (stop     . ,(pregexp "^;*@stop\\s*$"))
                  (args     . ,(pregexp "^;*@args(\\s+|$)"))
                  (schmooz . ,(pregexp "^;*@(\\s+|$)")))))
    (lambda (comments extracted)
      (let loop ((mode 'skip)
                 (args #f)
                 (collected '())
                 (spedls '())
                 (comments comments))
        (define (generate)
          (let ((texi-fragment (string-join (reverse collected) nl)))
            (guard
                (c ((parser-error? c)
                    (raise-extract-error
                     "failed to parse texinfo fragment: ~a: ~s~%\"~%~a~%\""
                     (condition-message c) (condition-irritants c)
                     texi-fragment)))
              (let ((stexi (cdr (texi-fragment->stexi texi-fragment))))
                (cond ((and (not (null? extracted)) (eq? mode 'schmooz))
                       `(group ,(arg-extended-items extracted args)
                               (documentation ,@stexi)))
                      ((not (null? stexi))
                       `(documentation ,@stexi))
                      (else #f))))))
        (cond
         ((and (null? comments) (null? collected))
          (filter values spedls))
         ((null? comments)
          (loop mode args '() (cons (generate) spedls) comments))
         (else
          (let* ((line (car comments))
                 (line-kind (let next-start ((starts starts))
                                 (if (null? starts)
                                     'regular
                                     (if (pregexp-match (cdar starts) line)
                                         (caar starts)
                                         (next-start (cdr starts)))))))
            (if (eq? line-kind 'stop)
                (loop 'skip args '() (cons (generate) spedls) (cdr comments))
                (case mode
                  ((skip)
                   (if (eq? line-kind 'regular)
                       (loop 'skip #f collected spedls (cdr comments))
                       (loop line-kind
                             #f
                             collected
                             spedls
                             comments)))
                  ((verbatim)
                   (if (eq? line-kind 'schmooz)
                       (loop 'schmooz args '() (cons (generate) spedls) comments)
                       (loop mode
                             #f
                             (cons (string-trim line #\;) collected)
                             spedls
                             (cdr comments))))
                  ((args)
                   (loop 'schmooz
                         (let ((space-pos (string-index line #\space)))
                           (and space-pos
                                (list->vector
                                 (string-tokenize
                                  (substring/shared line
                                                    (+ space-pos 1)
                                                    (string-length line))
                                  char-set:graphic))))
                         collected
                         spedls
                         (cdr comments)))
                  ((schmooz)
                   (let ((line (schmooz-trim line)))
                     (cond
                      ((match* "@[0-9]+" line)
                       => (lambda (matches)
                            (define (extracted-args)
                              (cond ((and (not (null? extracted))
                                          ((sxpath '(// @ arguments)) (car extracted)))
                                     => (lambda (as)
                                          (arglist->str-vector (cdar as))))
                                    (else #f)))
                            (loop mode
                                  args
                                  (cons (expand-arg-macros matches
                                                           line
                                                           (or args (extracted-args))
                                                           (car extracted))
                                        collected)
                                  spedls
                                  (cdr comments))))
                      (else
                       (loop mode args (cons line collected) spedls (cdr comments)))))))))))))))

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
