(define nl (string #\newline))
(define (delq key alist)
  (remove (lambda (entry) (eq? key (car entry))) alist))

(define xhtml-doctype
  (string-append
   "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" "
   "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
   nl nl))

(define (spedl->html title spedl directory)
  (call-with-output-file (make-path directory "index.html")
    (lambda (port)
      (display xhtml-doctype port)
      (sxml->xml (pre-post-order
                  spedl
                  (systems->html-rules title "." "../spe-doc"))
                 port))))


(define (systems->html title html-dir . sys-defs)
  (let ((spedl (apply systems->spedl sys-defs)))
    (spedl->html title spedl html-dir)
    (spedl->structure-html title spedl html-dir)))

(define (for-each-structure proc spedl)
  (let ((structures ((sxpath '(group items system items)) spedl)))
    (pre-post-order
     structures
     `((items
        ((group *macro* . ,(lambda (tag . subs)
                             (let ((items (assq-ref subs 'items))
                                   (docs (assq 'documentation subs)))
                               `(*structures
                                 ,@(filter-map
                                    (lambda (item)
                                      (and (pair? item)
                                           (eq? (car item) 'structure)
                                           `(*group ,item ,docs)))
                                    items)))))
         (*structures
          ((*group
            ((structure
              ((@ *preorder* . ,list)
               (files *preorder* .
                      ,(lambda (tag . files)
                         `(items ,@(snarf-files usual-spedl-extractors
                                                files))))
               (*default* *preorder* . ,list))
              . ,list)
             (documentation *preorder* . ,list)
             (*default* *preorder* . ,(lambda args (write args) (newline) args)))
            . ,(lambda (tag . subs)
                 (proc (assq 'structure subs)
                       (assq-ref subs 'documentation)))))
          . ,(lambda args '())))
        . ,(lambda args '()))))))

(define (spedl->structure-html title spedl html-dir)
  (for-each-structure
   (lambda (struct stexi)
     (let ((name (car (assq-ref (cdadr struct) 'name))))
       (emit-structure-html title name (cddr struct) stexi html-dir)))
   spedl))

(define (emit-structure-html title name subs docs directory)
  (let ((items (assq-ref subs 'items))
        (name-str (symbol->string name)))
    (if items
        (call-with-output-file
            (make-path directory
                       (append-extension name-str  "html"))
          (lambda (port)
            (display xhtml-doctype port)
            (sxml->xml
             (wrap-html title
                        "."
                        "../spe-doc.html"
                        (stexi->shtml
                         (spedl->stexi `(group (items
                                                (structure (@ (name ,name))
                                                           ,@subs))
                                               (documentation ,@docs)))))
             port))))))
                                 
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
                        `(dt ,@(concatenate
                                (filter-map
                                 (lambda (struct)
                                   (and (pair? struct)
                                        (eq? (car struct) 'structure)
                                        `(,(markup-structure-name
                                            (car (assq-ref (cdadr struct) 'name)))
                                          (br))))
                                 structures)))))
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
