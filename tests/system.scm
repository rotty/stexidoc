(define *foo-dir* (make-path *test-root-dir* "systems" "foo"))

(testeez "External system"
  (test/equal "loading"
    (systems->spedl (make-path *foo-dir* "sys-def.scm"))
    `(items
      (documentation (subsection "Metasyntactics"))
      (group
       (items
        (system
         (@ (name foo) (version (0 1 0)))
         (items (group
                 (items
                  (structure (@ (name foo.qux))
                             (interface (export quizzy quazzy))
                             (files ,(make-path *foo-dir* "qux.scm"))))
                 (documentation
                  (para "Provides quxy methods for ensuring fooish behaviour"))))))
       (documentation (para "Contains facilities to ensure fooish behaviour")))
      (documentation (subsection "Blah"))
      (group
       (items (system (@ (name blah) (version (0 1 0)))))
       (documentation (para "Blah, blah..."))))))
