((systems stexidoc)
 (files
  ("read.scm" (stexidoc read-r5rs) (srfi :13 strings) (rnrs io ports))
  "extract.scm"
  #;("system.scm" (stexidoc system) (spells pathname) (srfi :13 strings))
  ("texi.scm" (stexidoc texi) (stexidoc extract) (srfi :13 strings) (rnrs io ports))
  #;("html.scm" (stexidoc html) (spells pathname) (spells filesys) (rnrs control))
  ))
