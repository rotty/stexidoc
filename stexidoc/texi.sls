;;; texi.sls --- convert stexidoc to plain stexi

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

#!r6rs

(library (stexidoc texi)
  (export spedl->stexi)
  (import (except (rnrs base)
                  error string-copy string->list string-for-each
                  map for-each)
          (srfi :1 lists)
          (spells alist)
          (spells misc)
          (spells format)
          (spells match)
          (wak fmt)
          (spells tracing)
          (spells include)
          (xitomatl ssax tree-trans)
          (xitomatl sxml-tools sxpath)
          (stexidoc util))

  (include-file ((stexidoc private) texi))

  )
