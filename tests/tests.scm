((systems stexidoc)
 (files
  ("read.scm" stexidoc.read-r5rs spells.strings rnrs.io.ports)
  ("extract.scm" stexidoc.extract spells.strings rnrs.io.ports)
  ("system.scm" stexidoc.system spells.pathname spells.strings)
  ("texi.scm" stexidoc.texi stexidoc.extract spells.strings rnrs.io.ports)
  ("html.scm" stexidoc.html spells.pathname spells.filesys rnrs.control)
  ))

