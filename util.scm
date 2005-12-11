(define (snarf-files extractors files)
  (append-map
   (lambda (file)
     (pre-post-order
      (call-with-input-file file
        (lambda (port)
          (scheme->spedl extractors port)))
      `((*fragment* *preorder* . ,(lambda (tag . subs) subs))
        ,@universal-spedl-rules)))
   files))

;; arch-tag: 093180ca-4e45-4f17-8ecc-58b10e4d16b9
