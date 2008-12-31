(define (line-port . lines)
  (open-string-input-port (string-join lines (string #\newline))))

(define (stexi . lines)
  (spedl->stexi (scheme->spedl usual-spedl-extractors
                               (apply line-port lines))))

(testeez "tests"
  (test/equal "defvarx"
    (stexi ";;@ Some docs."
           "(define foo 1)"
           "(define bar 2)")
    '(*fragment* (defvar (% (name "foo"))
                   (defvarx (% (name "bar")))
                   (para "Some docs."))))
  (test/equal "structure"
    (spedl->stexi '(*fragment* (group (items
                                       (structure (^ (name foo))
                                                  (interface (export bar))
                                                  (items
                                                   (group
                                                    (items
                                                     (procedure (^ (name bar)
                                                                   (arguments x))))
                                                    (documentation (para "Bar"))))))
                                      (documentation (para "Hello")))))
    '(*fragment* (section "Overview")
                 (para "Hello")
                 (section "Usage")
                 (defun (% (name "bar") (arguments "x"))
                   (para "Bar")))))

