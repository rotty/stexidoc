;;; texi.scm --- tests for stexidoc to stexinfo conversion

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

(import (rnrs)
        (only (srfi :13) string-join)
        (spells testing)
        (stexidoc texi)
        (stexidoc extract))

(define (line-port . lines)
  (open-string-input-port (string-join lines (string #\newline))))

(define (stexi . lines)
  (spedl->stexi (scheme->spedl usual-spedl-extractors
                               (apply line-port lines))))

(define-test-suite texi-tests
  "Texinfo parsing")

(define-test-case texi-tests defvarx ()
  (test-equal '(*fragment*
                (anchor (% (name "defvar-foo")))
                (defvar (% (name "foo"))
                        (anchor (% (name "defvarx-bar")))
                        (defvarx (% (name "bar")))
                        (para "Some docs.")))
    (stexi ";;@ Some docs."
           "(define foo 1)"
           "(define bar 2)")))

(define-test-case texi-tests structure ()
  (test-equal '(*fragment* (node (% (name "(foo bar)")))
                           (para "Hello")
                           (para "Blah, blah...")
                           (anchor (% (name "defun-bar")))
                           (defun (% (name "bar") (arguments "x"))
                                  (para "Bar")))
    (spedl->stexi '(group (items
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
