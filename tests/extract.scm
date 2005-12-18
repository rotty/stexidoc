(define (line-port . lines)
  (open-input-string (string-join lines (string #\newline))))

(testeez "verbatim"
  (test/equal "single/eof"
    (scheme->spedl `() (line-port ";;@section test"))
    '(*fragment* (documentation (section "test"))))
  (test/equal "single"
    (scheme->spedl `() (line-port ";;@section test"
                                  ";; some words"))
    '(*fragment* (documentation (section "test") (para "some words"))))
  (test/equal "multiple/sexp"
    (scheme->spedl '() (line-port ";;@section Foo Bar"
                                  ";;   Text, and more text"
                                  "(define foo 1)"
                                  ";;@subsection Baz"
                                  ";; bazzy text"
                                  "(define baz 'qux)"))
    '(*fragment* (documentation
                  (section "Foo Bar")
                  (para "Text, and more text")
                  (subsection "Baz")
                  (para "bazzy text")))))

(testeez "usual extractors"
  (test/equal "procedure"
    (scheme->spedl usual-spedl-extractors
                   (line-port ";;@ A function"
                              "(define (foo x y) (* x y))"))
    '(*fragment* (group (items (procedure (@ (name foo) (arguments x y))))
                        (documentation (para "A function")))))
  (test/equal "variable"
    (scheme->spedl usual-spedl-extractors
                   (line-port ";;@ A variable"
                              "(define foo 'bar)"))
    '(*fragment* (group (items (variable (@ (name foo))))
                        (documentation (para "A variable"))))))

(testeez "comment mix"
  (test/equal "single"
    (scheme->spedl usual-spedl-extractors
                   (line-port ";; Some non-doc comment"
                              ";;@section Foo and Bar"
                              ";;@ A variable"
                              "(define foo 'bar)"))
    '(*fragment* (documentation (section "Foo and Bar"))
                 (group (items (variable (@ (name foo))))
                        (documentation (para "A variable")))))
  (test/equal "multiple"
    (scheme->spedl usual-spedl-extractors
                   (line-port ";; Some non-doc comment"
                              ";;@section Foo"
                              ";;@ About foos"
                              "(define foo 1)"
                              "(define (make-foo x) (cons foo x))"
                              ";;@section Bar"
                              "(define (bar x) x)"))
    '(*fragment* (documentation (section "Foo"))
                 (group (items (variable (@ (name foo)))
                               (procedure (@ (name make-foo) (arguments x))))
                        (documentation (para "About foos")))
                 (documentation (section "Bar")))))
