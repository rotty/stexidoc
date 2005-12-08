(translate "=root" (string-append (this-directory) "/../.."))

(config '(load "=root/testeez/packages.scm"))
(user '(open testeez.tests))
(user '(open testeez.run))

(config '(load "=root/spells/packages.scm"))
(for-each (lambda (module) (user `(open ,module)))
          '(srfi-6 spedoc.sml))
(user `(run (run-tests-and-exit (list ,(this-directory)) ',(the-dialect))))

;; arch-tag: 5e0e20f4-e0f6-40c6-a91e-12c9ffe22efe
