;;; extract.scm --- stexidoc documentation extractor

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:


(define universal-spedl-rules
  `((*TEXT* . ,(lambda (tag text) text))
    (*DEFAULT* . ,(lambda args args))))

(define extractors-irx
  (irregex '(: bos (* space) (* ";") "@extractors" (+ space) ($ (* any)))))

(define (scheme->spedl extractors port-or-code)
  (define (generate comments extracted)
    (schmooz (reverse comments) (reverse extracted)))
  (loop continue ((with comments '())
                  (with extracted '())
                  (with spedls '())
                  (with extractors extractors)
                  (with code
                        (if (input-port? port-or-code)
                            (read-scheme-code port-or-code)
                            port-or-code)
                        (cdr code))
                  (until (null? code)))
    => `(*fragment* ,@(reverse (append (generate comments extracted) spedls)))
    (cond ((non-form? (car code))
           (case (non-form-type (car code))
             ((comment)
              (let ((comment (non-form-data (car code))))
                (define (eval-extractors+continue str)
                  (with-extract-error-handler comment
                      (lambda (c)
                        (continue (=> comments '())
                                  (=> extracted '())))
                    (lambda ()
                      (continue (=> extractors (eval-extractors str))))))
                (cond ((irregex-search extractors-irx comment)
                       => (lambda (match)
                            (eval-extractors+continue
                             (irregex-match-substring match 1))))
                      ((not (null? extracted))
                       (continue
                        (=> comments '())
                        (=> extracted '())
                        (=> spedls (append (generate comments extracted)
                                           spedls))
                        (=> code code)))
                  (else
                   (continue (=> comments (cons comment comments)))))))
             (else
              (continue))))
          (else
           (let ((form (car code)))
             (with-extract-error-handler form
                 (lambda (c)
                   (continue (=> comments '())
                             (=> extracted '())))
               (lambda ()
                 (cond ((and (pair? form)
                             (assq-ref extractors (car form)))
                        => (lambda (extractor)
                             (continue
                              (=> extracted
                                  (append (extractor form) extracted)))))
                       (else
                        (continue))))))))))

(define (with-extract-error-handler form handler thunk)
  (guard (c ((extract-error? c)
             ;;++ use raise-continuable?
             (display-condition
              (condition
               (make-extract-error)
               (make-message-condition (format #f "while processing ~s" form))
               (make-stacked-condition c))
              (current-error-port))
             (handler c)))
    (thunk)))

(define (eval-extractors str)
  (let* ((port (open-string-input-port str))
         (imports (read port))
         (expression (read port)))
    (match imports
      (('import libraries ___)
       (guard (c (#t
                  (raise
                    (condition
                     (make-extract-error)
                     (make-message-condition "error evaluating extractors")
                     (make-irritants-condition (list imports expression))
                     (make-stacked-condition c)))))
         (eval expression (apply environment libraries))))
      (_
       (raise (condition
               (make-extract-error)
               (make-message-condition "invalid import specification")
               (make-irritants-condition (list imports))))))))

(define (file-processing-error file c)
  (condition
   (make-error)
   (make-message-condition (string-substitute
                            "while processing file {0}"
                            (vector (x->namestring file))))
   (make-stacked-condition c)))

(define (raise-file-processing-error file c)
  (raise (file-processing-error file c)))

(define (files->spedl extractors files)
  (append-map
   (lambda (file)
     (cdr
      (guard (c ((parser-error? c)
                 (raise-file-processing-error file c))
                ((extract-error? c)
                 (raise-file-processing-error file c)))
        (call-with-input-file (x->namestring file)
          (lambda (port)
            (scheme->spedl extractors port))))))
   files))

(define-condition-type &extract-error &error
  make-extract-error extract-error?)

(define (raise-extract-error msg . args)
  (raise (condition (make-extract-error)
                    (make-message-condition (apply format #f msg args)))))

(define (argspec->str spec)
  (define (lose)
    (raise-extract-error "invalid argument spec ~s" spec))
  (cond ((symbol? spec) (symbol->string spec))
        ((pair? spec)
         (case (car spec)
           ((rest-list optional) (symbol->string (cadr spec)))
           (else
            (lose))))
        (else
         (lose))))

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
  (let ((name (cadar ((sxpath '(// ^ name)) code))))
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
                           "}"))))))))

(define (match* pat str)
  (let loop ((pos 0) (matches '()))
    (cond ((and (< pos (string-length str))
                (irregex-search pat str pos))
           => (lambda (match)
                (let ((start (irregex-match-start-index match 0))
                      (end (irregex-match-end-index match 0)))
                  (loop end (cons (cons start end) matches)))))
          (else
           (and (not (null? matches)) (reverse matches))))))

(define (arg-extended-items items args)
  `(items
    ,@(cond (args
             (map (lambda (item)
                    (match item
                      ((name ('^ . attributes) . clauses)
                       `(,name
                         (^ (arguments
                             ,@(map string->symbol (vector->list args)))
                            ,@(filter (lambda (x)
                                        (not (and (pair? x)
                                                  (eq? (car x) 'arguments))))
                                      attributes))
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
      (substring uncommented 1 (string-length uncommented)))
     (else
      uncommented))))

(define schmooz-line-kind
  (let ((starts `((verbatim   . ,(irregex "^\\s*;*\\s*@(def|(sub)*(section|heading))"))
                  (stop       . ,(irregex "^\\s*;*@stop\\s*$"))
                  (args       . ,(irregex "^\\s*;*@args(\\s+|$)"))
                  (schmooz    . ,(irregex "^\\s*;*@(\\s+|$)")))))
    (lambda (line)
      (loop continue ((for start (in-list starts)))
        => 'regular
        (if (irregex-search (cdr start) line)
            (car start)
            (continue))))))

(define (extracted-args extracted)
  (and-let* ((as (and (not (null? extracted))
                      ((sxpath '(// ^ arguments))
                       (car extracted)))))
    (arglist->str-vector (or (and (not (null? as))
                                  (cdar as))
                             '()))))

(define (parse-args-line line)
  (let ((space-pos (string-index line #\space)))
    (and space-pos
         (list->vector
          (string-tokenize
           (substring line (+ space-pos 1) (string-length line))
           char-set:graphic)))))

(define (schmooz comments extracted)
  (define (generate mode args collected)
    (let ((texi-fragment (string-join (reverse collected) "\n")))
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
  (loop continue ((with mode 'skip)
                  (with args #f)
                  (with collected '())
                  (with spedls '())
                  (with comments comments)
                  (until (and (null? comments) (null? collected))))
    => (remv #f spedls)
    (if (null? comments)
        (continue (=> collected '())
                  (=> spedls (cons (generate mode args collected) spedls)))
        (let* ((line (car comments))
               (line-kind (schmooz-line-kind line)))
          #;
          (fmt #t "mode: " mode " line kind: " line-kind " collected: " collected " comments: " comments "\n")
          (match (cons mode line-kind)
            ((_ . 'stop)
             (continue (=> mode 'skip)
                       (=> collected '())
                       (=> spedls (cons (generate mode args collected) spedls))
                       (=> comments (cdr comments))))
            (('skip . 'regular)
             (continue (=> mode 'skip)
                       (=> args #f)
                       (=> comments (cdr comments))))
            (('skip . line-kind)
             (continue (=> mode line-kind)
                       (=> args #f)))
            (('verbatim . 'schmooz)
             (continue (=> mode 'schmooz)
                       (=> collected '())
                       (=> spedls (cons (generate mode args collected) spedls))))
            (('verbatim . line-kind)
             (continue (=> args #f)
                       (=> collected (cons (string-trim line #\;) collected))
                       (=> comments (cdr comments))))
            (('args . line-kind)
             (continue (=> mode 'schmooz)
                       (=> args (parse-args-line line))
                       (=> comments (cdr comments))))
            (('schmooz . line-kind)
             (let ((line (schmooz-trim line)))
               (cond
                 ((match* "@[0-9]+" line)
                  => (lambda (matches)
                       (continue
                        (=> collected
                            (cons (expand-arg-macros
                                   matches
                                   line
                                   (or args (extracted-args extracted))
                                   (car extracted))
                                  collected))
                        (=> comments (cdr comments)))))
                 (else
                  (continue (=> collected (cons line collected))
                            (=> comments (cdr comments))))))))))))

(define (extend-extractors extractors extensions)
  (loop continue ((for extension (in-list extensions))
                  (with originals extractors)
                  (with combined '()))
    => (append combined originals)
    (match extension
      ((name . (? procedure? extractor))
       (cond ((assq name originals)
              => (lambda (original)
                   (continue
                    (=> combined
                        (cons (cons name (combine-extractors extractor
                                                             (cdr original)))
                              combined))
                    (=> originals (remq original originals)))))
             (else
              (continue (=> combined (cons extension originals)))))))))

(define (combine-extractors first second)
  (lambda (form)
    (cond ((first form) => values)
          (else        (second form)))))

(define (extract-define form)
  (match (cdr (strip-non-forms form))
    (((name . args) . body)
     `((procedure (^ (name ,name) (arguments ,@(args->proper-list args))))))
    ((name ('lambda args . body))
     `((procedure (^ (name ,name) (arguments ,@(args->proper-list args))))))
    ((name ('case-lambda . clauses))
     (map (lambda (clause)
            (match clause
              ((arguments . body)
               `(procedure (^ (name ,name) (arguments ,@arguments))))
              (else
               (raise-extract-error "unmatched define with case-lambda"))))
          clauses))
    ((name expr)
     `((variable (^ (name ,name)))))
    (else
     (raise-extract-error "unmatched define"))))

(define (extract-define-syntax form)
  (match (cdr (strip-non-forms form))
    ((name expr)
     `((syntax (^ (name ,name)))))
    (else
     (raise-extract-error "unmatched define-syntax"))))

(define (extract-define/optional-args form)
  (match (cadr (strip-non-forms form))
    ((name . args)
     `((procedure
        (^ (name ,name)
           (arguments ,@(fold-right
                         (lambda (arg rest)
                           (cond ((and (pair? arg) (eq? (car arg) 'optional))
                                  (cons `(optional ,@(map car (cdr arg)))
                                        rest))
                                 (else (cons arg rest))))
                         '()
                         args))))))
    (else
     (raise-extract-error "unmatched define/optional-args"))))

(define usual-spedl-extractors
  `((define . ,extract-define)
    (define-syntax . ,extract-define-syntax)
    (define/optional-args . ,extract-define/optional-args)))

(define (args->proper-list args)
  (let loop ((result '()) (lst args))
    (cond ((null? lst)  (reverse result))
          ((pair? lst)  (loop (cons (car lst) result) (cdr lst)))
          (else         (loop (cons `(rest-list ,lst) result) '())))))


;; Local Variables:
;; scheme-indent-styles: (foof-loop (match 1))
;; End:
