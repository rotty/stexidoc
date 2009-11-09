(define (line-port . lines)
  (open-string-input-port (string-join lines (string #\newline))))

(define (comment text)
  (make-non-form `(comment ,text)))

(define-test-suite read-tests
  "Reading Scheme code")

(define-test-case read-tests single-form ()
  (test-equal '((foo 1 2 'bar))
    (read-scheme-code (line-port "(foo 1 2 'bar)"))))

(define-test-case read-tests form-and-comment ()
  (test-equal `((foo 42)
                ,(comment "; Hi there!"))
   (read-scheme-code (line-port "(foo 42)"
                                ";; Hi there!"))))

(define-test-case read-tests nested-comment ()
  (test-equal `((library (foo)
                  ,(comment "; FIXME: implement")))
    (read-scheme-code (line-port "(library (foo)"
                                 "  ;; FIXME: implement"
                                 ")"))))

(run-test-suite read-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing)
;; End:
