((systems stexidoc)
 (files
  ("read.scm" (stexidoc reader) (srfi :13 strings) (rnrs io ports))
  "extract.scm"
  #;("system.scm" (stexidoc system) (spells pathname) (srfi :13 strings))
  "texi.scm"
  #;("html.scm" (stexidoc html) (spells pathname) (spells filesys) (rnrs control))
  ))
