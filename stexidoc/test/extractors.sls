;;; extractors.sls --- stexidoc extractors for the test suite

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
#!r6rs

(library (stexidoc test extractors)
  (export test-extractors)
  (import (rnrs)
          (spells match)
          (stexidoc reader)
          (stexidoc extract))

(define (test-extractor:define form)
  (match (cdr (strip-non-forms form))
    ((name ('make-frobber))
     `((procedure (^ (name ,name) (arguments x y z)))))
    (_
     #f)))

(define test-extractors
  (extend-extractors usual-spedl-extractors
                     `((define . ,test-extractor:define))))

)

;; Local Variables:
;; scheme-indent-styles: ((match 1))
;; End:
