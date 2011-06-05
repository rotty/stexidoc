;;; texi.scm --- tests for stexidoc to stexinfo conversion

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
        (only (srfi :13) string-join)
        (wak trc-testing)
        (stexidoc renderer texinfo)
        (stexidoc extract))

(define (line-port . lines)
  (open-string-input-port (string-join lines (string #\newline))))

(define (stexi . lines)
  (stdl->stexi (scheme->spedl usual-spedl-extractors
                               (apply line-port lines))))

(define-test-suite texi-tests
  "Texinfo parsing")

(define-test-case texi-tests defun-args ()
  (test-equal '(*fragment*
                (anchor (% (name "foo")))
                (defun (% (name "foo")
                          (arguments "x" "y" "z"))
                       (para "Some docs.")))
    (stexi ";;@ Some docs."
           "(define (foo x y z) #f)")))

(define-test-case texi-tests defvarx ()
  (test-equal '(*fragment*
                (anchor (% (name "foo")))
                (defvar (% (name "foo"))
                        (anchor (% (name "bar")))
                        (defvarx (% (name "bar")))
                        (para "Some docs.")))
    (stexi ";;@ Some docs."
           "(define foo 1)"
           "(define bar 2)")))

(define-test-case texi-tests structure ()
  (test-equal '(*fragment* (node (% (name "(foo bar)")))
                           (section "(foo bar)")
                           (para "Hello")
                           (para "Blah, blah...")
                           (anchor (% (name "(foo bar) bar")))
                           (defun (% (name "bar") (arguments "x"))
                                  (para "Bar")))
    (stdl->stexi '(group (items
                           (structure (^ (name (foo bar)))
                                      (interface (export bar))
                                      (items
                                       (documentation (para "Blah, blah..."))
                                       (group
                                        (items
                                         (procedure (^ (name bar)
                                                       (arguments x))))
                                        (documentation (para "Bar"))))))
                          (documentation (para "Hello"))))))

(run-test-suite texi-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing)
;; End:
