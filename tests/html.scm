;;; html.scm --- Tests for the stexidoc HTML output

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
        (spells testing)
        (texinfo html)
        (stexidoc html))

(define-test-suite html-tests
  "HTML output")

(define-test-case html-tests basics ()
  (test-equal (stexi->shtml '(*fragment*
                              (para "Hello")
                              (para "Blah, blah...")
                              (defun (% (name "bar") (arguments "x"))
                                     (para "Bar"))))
    (stdl->shtml '(group (items
                          (structure (^ (name foo))
                                     (interface (export bar))
                                     (items
                                      (documentation (para "Blah, blah..."))
                                      (group
                                       (items
                                        (procedure (^ (name bar)
                                                      (arguments x))))
                                       (documentation (para "Bar"))))))
                         (documentation (para "Hello"))))))

(run-test-suite html-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing)
;; End:
