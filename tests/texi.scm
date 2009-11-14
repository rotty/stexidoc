(import (rnrs)
        (only (srfi :13) string-join)
        (spells testing)
        (stexidoc texi)
        (stexidoc extract))

(define (line-port . lines)
  (open-string-input-port (string-join lines (string #\newline))))

(define (stexi . lines)
  (spedl->stexi (scheme->spedl usual-spedl-extractors
                               (apply line-port lines))))

(define-test-suite texi-tests
  "Texinfo parsing")

(define-test-case texi-tests defvarx ()
  (test-equal '(*fragment* (defvar (% (name "foo"))
                                   (defvarx (% (name "bar")))
                                   (para "Some docs.")))
    (stexi ";;@ Some docs."
           "(define foo 1)"
           "(define bar 2)")))

(define-test-case texi-tests structure ()
  (test-equal '(*fragment* (para "Hello")
                           (para "Blah, blah...")
                           (defun (% (name "bar") (arguments "x"))
                                  (para "Bar")))
    (spedl->stexi '(group (items
                           (structure (^ (name foo))
                                      (interface (export bar))
                                      (items
                                       (documentation (para "Blah, blah..."))
                                       (group
                                        (items
                                         (procedure (^ (name bar)
                                                       (arguments x))))
                                        (documentation (para "Bar"))))))
                          (documentation (para "Hello"))))))

(run-test-suite texi-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing)
;; End:
