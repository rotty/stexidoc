(define-structure stexidoc.extract (export scheme->spedl
                                         raise-extract-error
                                         usual-spedl-extractors
                                         universal-spedl-rules)
  (open scheme srfi-1 srfi-8 srfi-13 srfi-14 
        spells.ascii spells.misc spells.opt-args spells.error
        spells.format spells.parameter spells.condition spells.alist
        spells.match spells.pregexp spells.port
        sxml.sxpath
        texinfo.stexi)
  (files read extract))

(define-structure stexidoc.util (export snarf-files)
  (open scheme srfi-1 srfi-8
        spells.file
        sxml.transform
        stexidoc.extract)
  (files util))

(define-structure stexidoc.system (export systems->spedl interface-exported-names)
  (open scheme srfi-1 srfi-8
        spells.file spells.format spells.alist spells.error spells.match
        spells.table spells.parameter
        sxml.transform
        stexidoc.util stexidoc.extract)
  (files system))

(define-structure stexidoc.texi (export spedl->stexi)
  (open scheme srfi-1
        spells.alist spells.misc spells.format spells.match ;; debug
        sxml.transform
        stexidoc.system)
  (files texi))

(define-structure stexidoc.html (export systems->html)
  (open scheme srfi-1 srfi-8 srfi-13
        spells.file spells.alist spells.parameter spells.error
        sxml.simple sxml.transform sxml.sxpath
        texinfo.html
        stexidoc.util stexidoc.system stexidoc.texi stexidoc.extract)
  (files html))

;; arch-tag: 6f905e34-83d6-416d-9fbb-36da874c39e2
