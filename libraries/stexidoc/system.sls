#!r6rs

(library (stexidoc system)
  (export systems->spedl interface-exported-names)
  (import (except (rnrs base) error string-copy string->list string-for-each)
          (rnrs io simple)
          (rnrs io ports)
          (spells strings)
          (spells receive)
          (spells format)
          (spells alist)
          (spells lists)
          (only (spells error) make-error-signaller)
          (spells match)
          (spells table)
          (spells parameter)
          (spells pathname)
          (spells filesys)
          (spells tracing)
          (spells include)
          (sxml transform)
          (stexidoc util)
          (stexidoc extract))

  (define error (make-error-signaller "stexidoc system"))
  
  (include-file ((stexidoc scheme) system))

  )
