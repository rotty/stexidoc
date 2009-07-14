#!r6rs

(library (stexidoc system)
  (export systems->spedl interface-exported-names)
  (import (except (rnrs base)
                  error string-copy string->list string-for-each
                  map for-each)
          (rnrs io simple)
          (rnrs io ports)
          (srfi :13 strings)
          (srfi :8 receive)
          (spells format)
          (spells alist)
          (srfi :1 lists)
          (only (spells error) make-error-signaller)
          (spells match)
          (spells table)
          (srfi :39 parameters)
          (spells pathname)
          (spells filesys)
          (spells misc)
          (spells foof-loop)
          (spells tracing)
          (spells include)
          (xitomatl ssax tree-trans)
          (stexidoc util)
          (stexidoc extract)
          (stexidoc read-r5rs))

  (define error (make-error-signaller "stexidoc system"))
  
  (include-file ((stexidoc private) system))

  )
