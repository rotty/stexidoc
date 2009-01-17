(define (test-sys name)
  (pathname-join (this-directory)
                 (make-pathname #f (list "systems" name) #f)))

(define *bar-dir* (test-sys "bar"))

(define *out-dir* (pathname-as-directory ",test-html.tmp"))

(testeez "External system with R6RS libraries"
  (test-eval "no error?"
    (dynamic-wind
      (lambda ()
        (create-directory *out-dir*))
      (lambda ()
        (systems->html "Test" *out-dir*
                       (pathname-with-file *bar-dir* (make-file "sys-def" "scm"))))
      (lambda ()
        (directory-fold *out-dir* delete-file)
        (delete-file *out-dir*)))))
