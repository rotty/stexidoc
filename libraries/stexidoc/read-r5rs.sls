;;; read-r5rs.sls --- R5RS reader

;; Copyright (C) 2008, 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software; you can
;; redistribute it and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
#!r6rs

#!r6rs

(library (stexidoc read-r5rs)
  (export read-scheme-code
          non-form?
          make-non-form
          strip-non-forms)
  (import (except (rnrs base) error)
          (rnrs control)
          (rnrs unicode)
          (rnrs lists)
          (rnrs mutable-strings)
          (rnrs io simple)
          (rnrs exceptions)
          (rnrs conditions)
          (spells misc)
          (spells opt-args)
          (spells parameter)
          (spells delimited-readers)
          (spells condition)
          (only (spells error) make-error-signaller)
          (spells ascii)
          (spells format)
          (spells include))

  (define error (make-error-signaller "stexidoc R5RS reader"))

  (define (reverse-cons x lst)
    (let loop ((result x) (lst lst))
      (if (null? lst)
          result
          (loop (cons (car lst) result) (cdr lst)))))

  (define strip-non-forms
    (case-lambda
      ((lst n)
       (cond
        ((pair? lst)
         (let loop ((i 0) (result '()) (lst lst))
           (cond ((null? lst)
                  (reverse result))
                 ((or (not (pair? lst))
                      (and n (>= i n)))
                  (reverse-cons lst result))
                 ((non-form? (car lst))
                  (loop i result (cdr lst)))
                 ((pair? (car lst))
                  (loop (+ i 1)
                        (cons (strip-non-forms (car lst)) result)
                        (cdr lst)))
                 (else
                  (loop (+ i 1) (cons (car lst) result) (cdr lst))))))
        (else
         lst)))
      ((lst)
       (strip-non-forms lst #f))))

  (include-file ((stexidoc scheme) read))

  )
