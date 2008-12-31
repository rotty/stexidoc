;;; extract.sls --- stexidoc documentation extractor

;; Copyright (C) 2008 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (stexidoc extract)
  (export scheme->spedl
          raise-extract-error
          usual-spedl-extractors
          universal-spedl-rules)
  (import (except (rnrs base) error string-copy string->list string-for-each)
          (rnrs exceptions)
          (rnrs conditions)
          (rnrs lists)
          (spells misc)
          (spells opt-args)
          (only (spells error) make-error-signaller)
          (spells format)
          (spells parameter)
          (spells condition)
          (spells alist)
          (spells match)
          (spells strings)
          (spells char-set)
          (spells include)
          (xitomatl irregex)
          (sxml sxpath)
          (texinfo)
          (stexidoc read-r5rs))

  (define error (make-error-signaller "stexidoc extractor"))

  (include-file ((stexidoc scheme) extract))

  )
