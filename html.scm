(define nl (string #\newline))
(define (delq key alist)
  (remove (lambda (entry) (eq? key (car entry))) alist))

(define xhtml-doctype
  (string-append
   "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" "
   "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
   nl nl))

(define (spedl->html bundle-name spedl directory)
  (call-with-output-file (make-path directory "index.html")
    (lambda (port)
      (display xhtml-doctype port)
      (sxml->xml (pre-post-order
                  spedl
                  (systems->html-rules bundle-name directory
                                       (make-path directory "spe-doc")))
                 port))))


;                  ,(lambda (tag . subs)
;                     (let* ((system (assq-ref subs 'system))
;                            (system-name (car (assq-ref (cdar system) 'name)))
;                            (structures (assq-ref system 'structures))
;                            (docs (assq-ref subs 'documentation)))
;                       `((h3 ,(symbol->string system-name))
;                         ,(or (and (null? docs) "[Undocumented]")
;                              (stexi->shtml (car docs)))
;                         ,@(structures->shtml structures))))

(define (systems->html title output-dir . sys-defs)
  (spedl->html title (apply systems->spedl sys-defs) output-dir))

(define (systems->html-rules title root-path scm-url)
  `((items
     ((group *macro* . ,(lambda (tag . subs)
                          (let ((items (assq-ref subs 'items))
                                (docs (assq 'documentation subs)))
                            `(*systems
                              ,@(map (lambda (item) (append item (list docs)))
                                     items)))))
      (*systems
       ((system
         ((@ *preorder* . ,list)
          (items
           ((group
             ((items *preorder* .
                     ,(lambda (tag . structures)
                        `(dt ,@(append-map
                                (lambda (struct)
                                  `(,(markup-structure-name
                                      (car (assq-ref (cdadr struct) 'name)))
                                    (br)))
                                structures))))
              (documentation *preorder* .
                             ,(lambda (tag . stexi)
                                `(dd ,@(stexi->shtml stexi)))))
             . ,(lambda (tag . subs) subs)))
           . ,(lambda (tag . rows) `(dl (@ (id "structures")) ,@(concatenate rows))))
          (documentation *preorder* . ,(lambda (tag . stexi)
                                         `(documentation ,@(stexi->shtml stexi))))
          (*default* . ,(lambda args #f)))
         . ,(lambda (tag attr . subs)
              ;;(for-each display `(,tag ,attr ,@subs #\newline))
              (let* ((subs (filter values subs))
                     (name (car (assq-ref (cdr attr) 'name)))
                     (docs (assq-ref subs 'documentation)))
                (list (symbol->string name) docs (delq 'documentation subs))))))
       . ,(lambda (tag . systems)
            (list
             `(dl ,@(append-map
                     (lambda (s)
                       (let ((name (first s)))
                         `((dt (a (@ (href ,(string-append "#" name))) ,name))
                           (dd ,@(second s)))))
                     systems))
             (append-map (lambda (s)
                           `((h2 (a (@ (name ,(first s)))) ,(first s))
                             ,@(second s)
                             (h3 "Structures")
                             ,@(cddr s)))
                         systems)))))
     . ,(lambda (tag overview . shtmls)
          (wrap-html title root-path scm-url
                     `((h2 "Overview")
                       ,@overview
                       ,@(concatenate shtmls)))))
    ,@universal-spedl-rules))

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
    `(a (@ (href ,(append-extension name "html")) (class "system")) ,name)))

(define (wrap-html title root-path scm-url body)
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
          (h1 (@ (id "heading")) ,title)
          (div (@ (id "content"))
               ,@body)
          (div (@ (id "footer"))
               "powered by "
               (a (@ (href ,scm-url)) "spe-doc"))))))

;; arch-tag: 405522a1-6061-4f34-a0ad-85aa8ceb4425
