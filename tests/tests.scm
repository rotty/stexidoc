((systems stexidoc)
 (files
  ("read.scm" stexidoc.read-r5rs srfi.:13.strings rnrs.io.ports testeez)
  ("extract.scm" stexidoc.extract srfi.:13.strings rnrs.io.ports testeez)
  ("system.scm" stexidoc.system spells.pathname srfi.:13.strings testeez)
  ("texi.scm" stexidoc.texi stexidoc.extract srfi.:13.strings rnrs.io.ports testeez)
  ("html.scm" stexidoc.html spells.pathname spells.filesys rnrs.control testeez)
  ))

