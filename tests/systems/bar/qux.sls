#!r6rs

;;@ Provides quxy methods for ensuring fooish behaviour
(library (bar qux)
  (export quizzy quazzy quxxy)
  (import (rnrs)
          (spells include))

  ;;@ Frobnicate @1 with @2 and @3.
  (define (quxxy a b c))
  
  (include-file (bar qux)))
