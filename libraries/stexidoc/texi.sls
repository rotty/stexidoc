#!r6rs

(library (stexidoc texi)
  (export spedl->stexi)
  (import (except (rnrs base) error string-copy string->list string-for-each)
          (rnrs lists)
          (srfi :1 lists)
          (spells alist)
          (spells misc)
          (spells format)
          (spells match)
          (spells include)
          (sxml transform)
          (sxml sxpath)
          (stexidoc system))

  (include-file ((stexidoc scheme) texi))

  )
