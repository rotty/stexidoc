(load-system 'testeez)
(load-system 'stexidoc)

(user '(open testeez))
(user '(open testeez.run))
(user `(run (define *test-root-dir* ,(this-directory))))

(define (main args)
  (let ((test-specs (call-with-input-file
                        (string-append (this-directory) "/tests.scm")
                      read))
        (tests (cdr args)))
    ;; open structures
    (for-each (lambda (test-spec)
                (if (or (null? tests) (member (car test-spec) tests))
                    (for-each (lambda (structure)
                                (user `(open ,structure)))
                              (cdr test-spec))))
              test-specs)
    ;; run
    (if (null? tests)
        (user `(run (run-tests-and-exit (list ,(this-directory))
                                        ',(the-dialect))))
        (user `(run (run-tests-and-exit
                     ',(map (lambda (test)
                              (string-append (this-directory) "/" test))
                            tests)
                     ',(the-dialect)))))))

