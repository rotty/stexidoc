;;; pkg-list.scm --- Package information for stexidoc

;; Copyright (C) 2009-2011 Andreas Rottmann <a.rottmann@gmx.at>
;; Please see file AUTHORS for license conditions.

(package (stexidoc (0))
  (depends (srfi)
           (spells)
           (wak-foof-loop)
           (wak-texinfo)
           (wak-ssax)
           (wak-sxml-tools))
  (libraries ("stexidoc" . sls)
             ("stexidoc" "private")))

;; Local Variables:
;; scheme-indent-styles: ((package 1))
;; End:
