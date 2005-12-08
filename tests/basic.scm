(let ((code ";; a comment\n(define (foo x) (* x 2))"))
  (testeez
   "basic"
   (test/equal
    "simple"
    (sml->scheme (scheme->sml (open-input-string code))) code)))


;; arch-tag: eb4b0913-8337-4c7c-bc02-ecee372b51af
