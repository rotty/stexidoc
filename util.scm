(define (collect-code+doc elts)
  (let loop ((codes '()) (docs '()) (elts elts))
    (if (null? elts)
        (values (reduce append '() codes) (reduce append '() docs))
        (let ((elt (car elts)))
          (cond ((procedure? elt)
                 (receive (c d) (elt)
                   (loop (if c (cons c codes) codes)
                         (if d (cons d docs) docs)
                         (cdr elts))))
                (else
                 (loop (cons (list elt) codes) docs (cdr elts))))))))

;; arch-tag: 093180ca-4e45-4f17-8ecc-58b10e4d16b9
