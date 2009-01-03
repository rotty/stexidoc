(define (raise-file-processing-error file c)
  (raise (condition
          (make-error)
          (make-message-condition (string-substitute
                                   "while processing file {0}"
                                   (vector (x->namestring file))))
          (make-stacked-condition c))))

(define (snarf-files extractors files)
  (append-map
   (lambda (file)
     (cdr
      (guard (c ((parser-error? c)
                 (raise-file-processing-error file c)))
        (call-with-input-file (x->namestring file)
          (lambda (port)
            (scheme->spedl extractors port))))))
   files))

(define (maybe-symbol->string x)
  (if (symbol? x) (symbol->string x) x))

;; arch-tag: 093180ca-4e45-4f17-8ecc-58b10e4d16b9
