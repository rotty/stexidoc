;;; tests.scm --- Lists test suite files

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>
;; Please see file AUTHORS for license conditions.

((systems stexidoc)
 (files
  ("read.scm" (stexidoc reader) (srfi :13 strings) (rnrs io ports))
  "extract.scm"
  "texi.scm"
  #;("html.scm" (stexidoc html) (spells pathname) (spells filesys) (rnrs control))
  ))
