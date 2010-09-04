;;; util.scm --- stexidoc utilities

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

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

(define (attlist-ref attlist key)
  (car (assq-ref (cdr attlist) key)))

(define (apush k v vals)
  (cond ((assq k vals)
         => (lambda (entry)
              (acons k (cons v (cdr entry)) (remq entry vals))))
        (else
         (acons k (list v) vals))))

(define (list-intersperse lst elem)
  (if (null? lst)
      lst
      (let loop ((l (cdr lst)) (result (cons (car lst) '())))
        (if (null? l)
            (reverse result)
            (loop (cdr l) (cons (car l) (cons elem result)))))))

(define (out port . args)
  (for-each (lambda (x)
              (display x port))
            args))

;;++ need to reformulate this, perhaps using `dsp-condition'
(define (format-exception e port)
  (define (format indent c)
    (when (message-condition? c)
      (out port (make-string indent #\space) (condition-message c)))
    (when (irritants-condition? c)
      (out port " [")
      (let loop ((is (condition-irritants c)))
        (unless (null? is)
          (write (car is) port)
          (unless (null? (cdr is))
            (out port " "))
          (loop (cdr is))))
      (out port "]"))
    (when (stacked-condition? c)
      (display ":" port))
    (display "\n" port)
    (if (stacked-condition? c)
        (format (+ indent 2) (next-condition c))))
  (format 0 e))

(define (maybe-symbol->string x)
  (if (symbol? x) (symbol->string x) x))

(define (merge-fragments a b)
  (unless (and (pair? a)
               (eq? '*fragment* (car a))
               (pair? b)
               (eq? '*fragment* (car b)))
    (assertion-violation 'merge-fragments "invalid arguments" a b))
  (cons '*fragment* (append (cdr a) (cdr b))))

(define (library-name->path/reverse name)
  (collect-list-reverse (for part (in-list name))
    (pct-encode (string->utf8 (symbol->string part))
                filename-safe-char-set)))

(define (library-name->path name)
  (reverse (library-name->path/reverse name)))

(define (library-name->pathname name base)
  (let ((path (library-name->path/reverse name)))
    (pathname-join base (make-pathname #f (reverse (cdr path)) (car path)))))

(define filename-safe-char-set
  (char-set-difference char-set:printing
                       (string->char-set ">:\"/\\|?*%*")))

(define (interface-exported-names interface)
  (loop continue ((for elt (in-list (assq-ref interface 'export)))
                  (with out '()))
    => out
    (cond ((and (pair? elt) (pair? (car elt)))
           (continue (=> out (append (car elt) out))))
          ((pair? elt)
           (continue (=> out (cons (car elt) out))))
          (else
           (continue (=> out (cons elt out)))))))

;; Local Variables:
;; scheme-indent-styles: (foof-loop)
;; End:

