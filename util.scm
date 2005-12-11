(define (snarf-files extractors files)
  (append-map
   (lambda (file)
     (cdr
      (call-with-input-file file
        (lambda (port)
          (scheme->spedl extractors port)))))
   files))

;; arch-tag: 093180ca-4e45-4f17-8ecc-58b10e4d16b9
