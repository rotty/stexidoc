#!r6rs

;;@ Provides quxy methods for ensuring fooish behaviour
(library (bar qux)
  (export quizzy quazzy)
  (import (rnrs)
          (spells include))
  
  (include-file (bar qux)))
