((systems stexidoc)
 (code
  (define *test-root-dir* (this-directory)))
 (files
  ("extract.scm" stexidoc.extract srfi-6 srfi-13)
  ("system.scm" stexidoc.system spells.pathname srfi-6 srfi-13)
  ("texi.scm" stexidoc.texi stexidoc.extract srfi-6 srfi-13)))

