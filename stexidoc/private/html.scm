;; -*- Mode: Scheme; scheme48-package: stexidoc.html; -*-

(define nl (string #\newline))
(define (delq key alist)
  (remove (lambda (entry) (eq? key (car entry))) alist))

(define xhtml-doctype
  (string-append
   "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" "
   "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
   nl nl))

(define (urlify str)
  (string-downcase
   (string-map
    (lambda (c)
      (case c
        ((#\space #\/ #\:) #\-)
        (else c)))
    str)))

(define (spedl->html title spedl directory)
  (call-with-output-file (x->namestring
                          (pathname-join (pathname-as-directory directory)
                                         "index.html"))
    (lambda (port)
      (display xhtml-doctype port)
      (sxml->xml (pre-post-order
                  spedl
                  (systems->html-rules title "." "../spe-doc"))
                 port))))

(define (spedl-resolve-ref node-name manual-name)
  (let* ((node-parts (string-tokenize node-name))
         (ref  (case (length node-parts)
                 ((1) (list "#" node-name))
                 ((2) (list (first node-parts) ".html#" (second node-parts)))
                 (else #f))))
    (and ref (urlify (string-concatenate (cons (or manual-name "")  ref))))))

(define (systems->html title html-dir . sys-defs)
  (let ((spedl (apply systems->spedl sys-defs)))
    (parameterize ((stexi-ref-resolvers (list spedl-resolve-ref)))
      (spedl->html title spedl html-dir)
      (spedl->structure-html title spedl html-dir))))

(define (for-each-structure proc spedl)
  (let* ((structures ((sxpath '(group items system items)) spedl))
         (paired (pair-struct-docs (car structures)))
         (expanded (expand-struct-files paired)))
    (for-each (lambda (group)
                (proc (cadr group)
                      (cdaddr group)))
              ((sxpath '(*structures *group)) expanded))))

(define (pair-struct-docs structures)
  (pre-post-order
   structures
   `((items
      ((group *PREORDER* .
              ,(lambda (tag . subs)
                 (let ((items (assq-ref subs 'items))
                       (docs (assq 'documentation subs)))
                   `(*structures
                     ,@(filter-map
                        (lambda (item)
                          (and (pair? item)
                               (eq? (car item) 'structure)
                               `(*group ,item ,docs)))
                        items)))))
       (documentation *PREORDER* . ,list))
      . ,list))))

(define (expand-struct-files structs)
  (pre-post-order
   structs
   `((items
      ((*structures
        ((*group
          ((structure
            ((^ *PREORDER* . ,list)
             (files *PREORDER* .
                    ,(lambda (tag . files)
                       `(items ,@(files->spedl usual-spedl-extractors
                                               files))))
             (*DEFAULT* *PREORDER* . ,list))
            . ,list)
           (documentation *PREORDER* . ,list))
          . ,list))
        . ,list))
      . ,list))))

(define (spedl->structure-html title spedl html-dir)
  (for-each-structure
   (lambda (struct stexi)
     (let ((name (car (assq-ref (cdadr struct) 'name))))
       (emit-structure-html title
                            name
                            (cddr struct)
                            stexi
                            (pathname-as-directory html-dir))))
   spedl))

(define (structname->string name)
  (cond ((symbol? name)
         (symbol->string name))
        (else
         (call-with-string-output-port
           (lambda (port)
             (write name port))))))

(define (intersperse l sep)
  (let loop ((l l) (r '()) (sep sep) (orig l))
    (cond ((pair? l) (loop (cdr l) (cons* sep (car l) r) sep orig))
          ((null? l) (if (null? r) '() (reverse (cdr r))))
          (else (error 'intersperse "not a proper list" orig)))))

(define (structname->namestring name)
  (cond ((symbol? name)
         (symbol->string name))
        (else
         (string-concatenate (intersperse (map symbol->string name) ".")))))

(define (emit-structure-html title name subs docs directory)
  (let ((items (assq-ref subs 'items))
        (name-str (structname->namestring name)))
    (call-with-output-file
        (x->namestring
         (pathname-with-file directory (make-file name-str "html")))
      (lambda (port)
        (let ((stexi (spedl->stexi `(*fragment*
                                     (group (items
                                             (structure (^ (name ,name))
                                                        ,@subs))
                                            (documentation ,@docs))))))
          (display xhtml-doctype port)
          (sxml->xml
           (wrap-html title
                      "."
                      "../spe-doc.html"
                      (map stexi->shtml (cdr stexi)))
           port))))))

(define (systems->html-rules title root-path scm-url)
  `((items
     ((group *MACRO* . ,(lambda (tag . subs)
                          (let ((items ((sxpath '(items *)) (cons tag subs)))
                                (docs (assq 'documentation subs)))
                            `(*systems
                              ,@(map (lambda (item) (append item (list docs)))
                                     items)))))
      (*systems
       ((system
         ((^ *PREORDER* . ,list)
          (items
           ((group
             ((items *PREORDER* .
                     ,(lambda (tag . subs)
                        (let ((markup (filter-map
                                       (lambda (sub)
                                         (and (pair? sub)
                                              (eq? (car sub) 'structure)
                                              `(,(markup-structure-name
                                                  (car (assq-ref (cdadr sub) 'name)))
                                                (br))))
                                       subs)))
                          (and (not (null? markup))
                               `(dt ,@(concatenate markup))))))
              (documentation *PREORDER* .
                             ,(lambda (tag . stexi)
                                `(dd ,@(stexi->shtml stexi)))))
             . ,(lambda (tag items docs)
                  `(data ,items ,docs)))
            (documentation *PREORDER* . ,(lambda (tag . stexi)
                                           `(break ,@(stexi->shtml stexi)))))
           . ,(lambda (tag . rows) (sectioned-list "structures" rows)))
          (documentation *PREORDER* . ,(lambda (tag . stexi)
                                         `(documentation ,@(stexi->shtml stexi))))
          (*DEFAULT* . ,(lambda args #f)))
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
                         `((dt (a (^ (href ,(string-append "#" name))) ,name))
                           (dd ,@(second s)))))
                     systems))
             (append-map (lambda (s)
                           `((h2 (a (^ (name ,(first s)))) ,(first s))
                             ,@(second s)
                             (h3 "Structures")
                             ,@(cddr s)))
                         systems)))))
     . ,(lambda (tag . subs)
          (wrap-html title root-path scm-url
                     `((h2 "Overview")
                       ,@(map first subs)
                       ,@(append-map second subs)))))
    ,@universal-spedl-rules))

(define (markup-structure-name name)
  (let ((name-str (structname->string name)))
    `(a (^ (href ,(string-append (structname->namestring name) ".html"))
           (class "system"))
        ,name-str)))

(define (sectioned-list id rows)
  (define (dl items)
    `(dl ,@(concatenate (reverse items))))
  (let loop ((items '()) (markup '()) (rows rows))
    (cond ((and (null? rows) (null? items))
           `(div (^ (id ,id)) ,@(reverse markup)))
          ((null? rows)
           (loop '() (cons (dl items) markup) rows))
          (else
           (case (caar rows)
             ((data)
              (loop (cons (cdar rows) items) markup (cdr rows)))
             ((break)
              (if (null? items)
                  (loop '() (append (cdar rows) markup) (cdr rows))
                  (loop '()
                        (append (cdar rows) (list (dl items)) markup)
                        (cdr rows))))
             (else
              (error "unexpected tag" (caar rows))))))))

(define (wrap-html title root-path scm-url body)
  `(html (^ (xmlns "http://www.w3.org/1999/xhtml"))
    (head
     (title ,title)
     (meta (^ (name "Generator")
              (content "SPE-doc, a Scheme documentation extractor")))
     (style (^ (type "text/css") (media "screen"))
       "@import url("
       ,(x->namestring (pathname-with-file root-path "screen.css"))
       ");"))
    (body
     (div (^ (id "body"))
          (h1 (^ (id "heading")) ,title)
          (div (^ (id "content"))
               ,@body)
          (div (^ (id "footer"))
               "powered by "
               (a (^ (href ,scm-url)) "spe-doc"))))))

;; arch-tag: 405522a1-6061-4f34-a0ad-85aa8ceb4425
