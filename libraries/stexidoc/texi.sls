#!r6rs

(library (stexidoc texi)
  (export spedl->stexi)
  (import (except (rnrs base) error string-copy string->list string-for-each)
          (spells lists)
          (spells alist)
          (spells misc)
          (spells format)
          (spells match)
          (spells include)
          (sxml transform)
          (stexidoc system))

  (include-file ((stexidoc scheme) texi))

  )
