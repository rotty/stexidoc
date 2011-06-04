;;; extract.scm --- tests for documentation extraction

;; Copyright (C) 2009, 2011 Andreas Rottmann <a.rottmann@gmx.at>

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

(import (rnrs)
        (only (srfi :13 strings) string-join)
        (wak trc-testing)
        (stexidoc extract))

(define (line-port . lines)
  (open-string-input-port (string-join lines (string #\newline))))

(define (usual-spedl . lines)
  (scheme->spedl usual-spedl-extractors
                 (apply line-port lines)))

(define-test-suite extract-tests
  "Extracting documentation")

(define-test-case extract-tests single/eof ()
  (test-equal '(*fragment* (documentation (section "test")))
    (scheme->spedl `() (line-port ";;@section test"))))

(define-test-case extract-tests single ()
  (test-equal '(*fragment* (documentation (section "test") (para "some words")))
    (scheme->spedl `() (line-port ";;@section test"
                                  ";; some words"))))

(define-test-case extract-tests multiple/sexp ()
  (test-equal '(*fragment* (documentation
                            (section "Foo Bar")
                            (para "Text, and more text")
                            (subsection "Baz")
                            (para "bazzy text")
                            (anchor (% (name "defvar-baz")))
                            (defvar (% (name "baz"))
                                    (para "This is baz"))))
    (scheme->spedl '() (line-port ";;@section Foo Bar"
                                  ";;   Text, and more text"
                                  "(define foo 1)"
                                  ";;@subsection Baz"
                                  ";; bazzy text"
                                  ";;@defvar baz"
                                  ";; This is baz"
                                  ";;@end defvar"
                                  "(define baz 'qux)"))))

(define-test-suite (extract-tests.usual extract-tests)
  "Usual extractors")

(define-test-case extract-tests.usual procedure ()
  (test-equal '(*fragment* (group (items (procedure (^ (name foo) (arguments x y))))
                                  (documentation (para "A function"))))
    (usual-spedl";;@ A function"
                "(define (foo x y) (* x y))")))

(define-test-case extract-tests.usual variable ()
  (test-equal '(*fragment* (group (items (variable (^ (name foo))))
                                  (documentation (para "A variable"))))
    (usual-spedl ";;@ A variable"
                 "(define foo 'bar)")))

(define-test-suite (extract-tests.comment-mix extract-tests)
  "Mixing documentation comments with normal comments")

(define-test-case extract-tests.comment-mix single ()
  (test-equal '(*fragment* (documentation (section "Foo and Bar"))
                           (group (items (variable (^ (name foo))))
                                  (documentation (para "A variable"))))
    (usual-spedl ";; Some non-doc comment"
                 ";;@section Foo and Bar"
                 ";;@ A variable"
                 "(define foo 'bar)")))

(define-test-case extract-tests.comment-mix multiple ()
  (test-equal '(*fragment* (documentation (section "Foo"))
                           (group (items (variable (^ (name foo)))
                                         (procedure (^ (name make-foo) (arguments x))))
                                  (documentation (para "About foos")))
                           (documentation (section "Bar")))
    (usual-spedl ";; Some non-doc comment"
                 ";;@section Foo"
                 ";;@ About foos"
                 "(define foo 1)"
                 "(define (make-foo x) (cons foo x))"
                 ";;@section Bar"
                 "(define (bar x) x)")))

(define-test-case extract-tests.comment-mix stop ()
  (test-equal '(*fragment* (group (items (procedure (^ (name func1) (arguments))))
                                  (documentation (para "Func1"))))
    (usual-spedl ";@ Func1"
                 "(define (func1) #t)"
                 ";@stop"
                 "(define (func2) #f)")))

(define-test-case extract-tests.comment-mix override ()
  (test-equal '(*fragment* (group (items (procedure (^ (name func1)
                                                       (arguments))))
                                  (documentation (para "Func1")))
                           (documentation
                            (anchor (% (name "defun-func2")))
                            (defun (% (name "func2") (arguments "x y z"))
                                   (para "This is func2"))))
    (usual-spedl ";@ Func1"
                 "(define (func1) #t)"
                 ";@defun func2 x y z"
                 "; This is func2"
                 ";@end defun"
                 "(define (func2) #f)")))

(define-test-suite (extract-tests.extractor-spec extract-tests)
  "Extractor specifications")

(define-test-case extract-tests.extractor-spec basics ()
  (test-equal '(*fragment*
                (group
                 (items (procedure (^ (name foo) (arguments x y z)))
                        (procedure (^ (name bar) (arguments x y z))))
                 (documentation (para "Frobbers"))))
    (usual-spedl
     ";@extractors (import (stexidoc test extractors)) test-extractors"
     ";@ Frobbers"
     "(define foo (make-frobber))"
     "(define bar (make-frobber))")))

(run-test-suite extract-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing)
;; End:
