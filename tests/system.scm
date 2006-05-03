(define *foo-dir* (make-pathname
                   #f
                   (append (pathname-directory (x->pathname *test-root-dir*))
                           (list "systems" "foo"))
                   #f))

;; Like EQUAL?, but dealing with pathnames
(define (equal*? obj1 obj2)
  (cond ((eqv? obj1 obj2) #t)
        ((pair? obj1)
         (and (pair? obj2)
              (equal*? (car obj1) (car obj2))
              (equal*? (cdr obj1) (cdr obj2))))
        ((string? obj1)
         (and (string? obj2)
              (string=? obj1 obj2)))
        ((vector? obj1)
         (and (vector? obj2)
              (let ((z (vector-length obj1)))
                (and (= z (vector-length obj2))
                     (let loop ((i 0))
                       (cond ((= i z) #t)
                             ((equal*? (vector-ref obj1 i) (vector-ref obj2 i))
                              (loop (+ i 1)))
                             (else #f)))))))
        ((pathname? obj1)
         (and (pathname? obj2) (pathname=? obj1 obj2)))
        (else #f)))

(testeez "External system"
  (test-true "loading"
    (equal*?
     (systems->spedl (pathname-with-file *foo-dir* (make-file "sys-def" "scm")))
     `(items
       (documentation (subsection "Metasyntactics"))
       (group
        (items
         (system
          (@ (name foo))
          (items (group
                  (items
                   (structure (@ (name foo.qux))
                              (interface (export quizzy quazzy))
                              (files ,(pathname-with-file *foo-dir* (make-file "qux" "scm")))))
                  (documentation
                   (para "Provides quxy methods for ensuring fooish behaviour"))))))
        (documentation (para "Contains facilities to ensure fooish behaviour")))
       (documentation (subsection "Blah"))
       (group
        (items (system (@ (name blah))))
        (documentation (para "Blah, blah...")))))))
