;;; util.sls --- utility library for stexidoc

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

(library (stexidoc util)
  (export list-intersperse
          attlist-ref
          format-exception
          maybe-symbol->string
          merge-fragments
          library-name->path
          library-name->pathname)
  (import (rnrs)
          (srfi :14 char-sets)
          (spells alist)
          (spells pathname)
          (spells condition)
          (spells foof-loop)
          (spells nested-foof-loop)
          (spells include)
          (ocelotl net pct-coding))

  (include-file ((stexidoc private) util))
  
  )
