(define (line-port . lines)
  (open-string-input-port (string-join lines (string #\newline))))

(define (comment text)
  (make-non-form `(comment ,text)))

(testeez "read-scheme-code"
  (test/equal "single form"
    (read-scheme-code (line-port "(foo 1 2 'bar)"))
    '((foo 1 2 'bar)))
  (test/equal "form and comment"
    (read-scheme-code (line-port "(foo 42)"
                                 ";; Hi there!"))
    `((foo 42)
      ,(comment "; Hi there!")))
  (test/equal "nested comment"
    (read-scheme-code (line-port "(library (foo)"
                                 "  ;; FIXME: implement"
                                 ")"))
    `((library (foo)
        ,(comment "; FIXME: implement")))))


