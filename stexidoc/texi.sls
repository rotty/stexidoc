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
          (spells tracing)
          (spells include)
          (xitomatl ssax tree-trans)
          (xitomatl sxml-tools sxpath)
          (stexidoc system))

  (include-file ((stexidoc private) texi))

  )
