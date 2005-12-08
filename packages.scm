(define-structure spedoc.util (export collect-code+doc)
  (open scheme srfi-1 srfi-8)
  (files util))

(define-structure spedoc.extract (export scheme->spedl
                                         usual-spedl-extractors)
  (open scheme srfi-1 srfi-8 srfi-13 srfi-23
        spells.ascii spells.misc spells.opt-args
        spells.format spells.parameter spells.condition
        spells.match spells.pregexp spells.port
        sxml.transform sxml.sxpath
        texinfo.stexi)
  (files read extract))

(define-structure spedoc.library (export library->spedl)
  (open scheme srfi-1 srfi-8
        spells.file spells.format
        sxml.transform
        spedoc.util spedoc.extract)
  (files library))

(define-structure spedoc.texi (export spedl->stexi)
  (open scheme srfi-1
        spells.alist spells.misc spells.format spells.match ;; debug
        sxml.transform)
  (files texi))

(define-structure spedoc.html (export spedl->html library->html)
  (open scheme srfi-8
        spells.file spells.alist 
        sxml.simple sxml.transform
        texinfo.html
        spedoc.util spedoc.library spedoc.texi)
  (files html))

;; arch-tag: 6f905e34-83d6-416d-9fbb-36da874c39e2
