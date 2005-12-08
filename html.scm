(define nl (string #\newline))

(define xhtml-doctype
  (string-append
   "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" "
   "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
   nl nl))

(define (spedl->html lib-name spedl directory)
  (call-with-output-file (make-path directory "index.html")
    (lambda (port)
      (display xhtml-doctype port)
      (sxml->xml 
       (pre-post-order
        spedl
        `((library . ,(lambda (tag . groups)
                        (wrap-html (string-append lib-name ": Structures")
                                   lib-name
                                   ".."
                                   "../spe-doc.html"
                                   `((table (@ (id "structures")) ,@groups)))))
          (group *preorder* .
                 ,(lambda (tag . subs)
                    (let* ((struct (assq-ref subs 'structure))
                           (struct-name (car (assq-ref (cdar struct) 'name)))
                           (docs (assq-ref subs 'documentation)))
                      (emit-structure-html! lib-name struct-name (cdr struct) docs directory)
                      `((tr
                         (td ,(markup-structure-name struct-name))
                         (td ,(or (and (null? docs) "[Undocumented]")
                                  (stexi->shtml (car docs)))))))))))
       port))))

(define (library->html lib-name src-dir output-dir)
  (spedl->html lib-name (library->spedl src-dir) output-dir))

(define (emit-structure-html! lib-name name subs docs directory)
  (let ((bindings (assq-ref subs 'bindings))
        (name-str (symbol->string name)))
    (if bindings
        (call-with-output-file
            (make-path directory
                       (append-extension name-str  "html"))
          (lambda (port)
            (display xhtml-doctype port)
            (sxml->xml
             (wrap-html name-str
                        `(a (@ href "index.html") "[" ,lib-name "]")
                        ".."
                        "../spe-doc.html"
                        (stexi->shtml
                         (spedl->stexi `(group (structure (@ (name ,name))
                                                          ,@subs)
                                               (documentation ,@docs)))))
             port))))))
                                 
(define (process-documentation tag . subs)
  (lambda ()
    (values #f (stexi->shtml subs))))

(define (markup-structure-name name)
  (let ((name (symbol->string name)))
    `(a (@ (href ,(append-extension name "html")) (class "structure")) ,name)))

(define (wrap-html title heading root-path scm-url body)
  `(html (@ (xmlns "http://www.w3.org/1999/xhtml"))
    (head
     (title ,title)
     (meta (@ (name "Generator")
              (content "SPE-doc, a Scheme documentation extractor")))
     (style (@ (type "text/css") (media "screen"))
       "@import url("
       ,(make-path root-path "screen.css")
       ");"))
    (body
     (div (@ (id "body"))
          (h1 (@ (id "heading")) ,heading)
          (div (@ (id "content"))
               (h2 (@ (class "centered")) ,title)
               ,@body)
          (div (@ (id "footer"))
               "powered by "
               (a (@ (href ,scm-url)) "spe-doc"))))))

;; arch-tag: 405522a1-6061-4f34-a0ad-85aa8ceb4425
