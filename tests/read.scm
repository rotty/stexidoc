;;; read.scm --- tests for the stexidoc Scheme reader

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

(define (line-port . lines)
  (open-string-input-port (string-join lines (string #\newline))))

(define (comment text)
  (make-non-form 'comment text))

(define (non-form=? x y)
  (and (eq? (non-form-type x)
            (non-form-type y))
       (equal? (non-form-data x)
               (non-form-data y))))

(define (form=? x y)
  (cond ((non-form? x)
         (and (non-form? y)
              (non-form=? x y)))
        ((non-form? y)
         #f)
        ((and (pair? x) (pair? y))
         (and (form=? (car x) (car y))
              (form=? (cdr x) (cdr y))))
        (else
         (equal? x y))))

(define-test-suite read-tests
  "Reading Scheme code")

(define-test-case read-tests single-form ()
  (test-compare form=? '((foo 1 2 'bar))
    (read-scheme-code (line-port "(foo 1 2 'bar)"))))

(define-test-case read-tests form-and-comment ()
  (test-compare form=? `((foo 42)
                ,(comment "; Hi there!"))
   (read-scheme-code (line-port "(foo 42)"
                                ";; Hi there!"))))

(define-test-case read-tests nested-comment ()
  (test-compare form=? `((library (foo)
                         ,(comment "; FIXME: implement")))
    (read-scheme-code (line-port "(library (foo)"
                                 "  ;; FIXME: implement"
                                 ")"))))

(run-test-suite read-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing)
;; End:
