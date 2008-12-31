;;; util.sls --- utility library for stexidoc

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

(library (stexidoc util)
  (export snarf-files)
  (import (except (rnrs base) error string-copy string->list string-for-each)
          (rnrs io simple)
          (spells lists)
          (spells pathname)
          (spells include)
          (sxml transform)
          (stexidoc extract))

  (include-file ((stexidoc scheme) util))
  
  )