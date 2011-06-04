;;; texi.sls --- convert stexidoc to plain stexi

;; Copyright (C) 2009-2011 Andreas Rottmann <a.rottmann@gmx.at>

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
  (export stdl->stexi
          library-identifier->node-name)
  (import (except (rnrs base) error map for-each)
          (srfi :1 lists)
          (only (srfi :13) string-prefix?)
          (spells alist)
          (spells misc)
          (spells format)
          (spells match)
          (wak fmt)
          (spells tracing) ;debug
          (spells include)
          (wak foof-loop)
          (wak foof-loop nested)
          (wak ssax tree-trans)
          (wak sxml-tools sxpath)
          (stexidoc util))

  (include-file ((stexidoc private) texi))

  )
