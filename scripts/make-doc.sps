(import (except (rnrs) delete-file file-exists?)
        (only (srfi :1) make-list)
        (only (srfi :13)
              string-join
              string-suffix?)
        (srfi :14 char-sets)
        (spells condition)
        (spells filesys)
        (spells pathname)
        (spells foof-loop)
        (spells nested-foof-loop)
        (spells fmt)
        (spells match)
        (spells alist)
        (xitomatl sxml-tools sxpath)
        (xitomatl ssax extras)
        (ocelotl net pct-coding)
        (dorodango package)
        (dorodango bundle)
        (stexidoc read-r5rs)
        (stexidoc util)
        (stexidoc extract)
        (stexidoc html))


;;; support for dorodango bundles and R6RS libraries

(define (bundle-package->stdl bundle package)
  (define (read-file pathname extractor)
    (guard (c ((parser-error? c)
               (fmt (current-output-port)
                    "While reading " (->namestring pathname) ":\n"
                    (dsp-condition c))
               '(*fragment*)))
      (scheme->spedl r6rs-toplevel-extractors
                     (transcoded-port
                      (open-bytevector-input-port
                       (call-with-bytevector-output-port extractor))
                      (native-transcoder)))))
  (loop ((for pathname extractor (in-bundle-inventory
                                  bundle
                                  (package-category-inventory package
                                                              'libraries)))
         (with stdl
               '(*fragment*)
               (if (appealing-pathname? pathname)
                   (merge-fragments (read-file pathname extractor)
                                    stdl)
                   stdl)))
    => (cons '*fragment* (reverse (cdr stdl)))))

(define (appealing-pathname? pathname)
  (let ((filename (file-namestring pathname)))
    (and (string-suffix? ".sls" filename)
         (not (exists (lambda (implementation)
                        (string-suffix? (string-append "." implementation ".sls")
                                        filename))
                      r6rs-implementations)))))

(define r6rs-implementations
  '("ikarus" "ypsilon" "larceny" "mzscheme" "mosh"))

(define (r6rs-library-extractor form)
  (match (strip-non-forms (cdr form) 2)
    ((name ('export . exports) ('import . imports) . forms)
     `((structure (^ (name ,name))
                  (interface (export ,@exports))
                  (items ,@(cdr (scheme->spedl usual-spedl-extractors
                                               forms))))))
    (else
     (raise-extract-error "unmatched LIBRARY"))))

(define r6rs-toplevel-extractors
  `((library . ,r6rs-library-extractor)))


;; HTML template

(define (wrap-html title heading root-path body)
  `(html (^ (xmlns "http://www.w3.org/1999/xhtml"))
    (head
     (title ,title)
     (meta (^ (name "Generator")
              (content "STexiDoc, a Scheme documentation extractor")))
     (style (^ (type "text/css") (media "screen"))
       "@import url("
       ,(x->namestring (pathname-with-file root-path "screen.css"))
       ");"))
    (body
     (div (^ (id "content"))
          (h1 (^ (id "heading")) ,heading)
          (div (^ (id "inner"))
               ,@body)
          (div (^ (id "footer"))
               "powered by "
               (a (^ (href ,stexidoc-homepage-url)) "stexidoc"))))))

(define (library-name->pathname name base)
  (let ((directory-parts (collect-list-reverse (for part (in-list name))
                           (pct-encode (string->utf8 (symbol->string part))
                                       filename-safe-char-set))))
    (pathname-join base (make-pathname #f
                                       (reverse (cdr directory-parts))
                                       (car directory-parts)))))

(define filename-safe-char-set
  (char-set-difference char-set:printing
                       (string->char-set ">:\"/\\|?*%*")))

(define stexidoc-homepage-url "http://github.com/rotty/stexidoc")

(define (make-back-filename n file)
  (string-append
   (if (= n 0)
       "."
       (string-join (make-list n "..") "/" 'suffix))
   (or file "")))

(define (library-shtml name library documentation)
  (wrap-html (fmt #f name)
             (library-name->heading name)
             (make-back-filename (length name) #f)
             (list (stdl->shtml `(group (items ,library)
                                        ,documentation)))))

(define (library-name->heading name)
  (let ((n-parts (length name)))
    (loop ((for i (down-from n-parts (to 0)))
           (for part (in-list name))
           (for result
                (appending
                 (if (= i 0)
                     (list part)
                     `((a (^ (href ,(make-back-filename i "index.html")))
                          ,part)
                       " ")))))
      => `(span "(" ,@result ")"))))

(define (generate-documentation bundle-filename output-directory)
  (let ((bundle (open-input-bundle bundle-filename)))
    (iterate! (for package (in-list (bundle-packages bundle)))
        (let stdl (bundle-package->stdl bundle package))
        (for library-group (in-list ((sxpath '(group)) stdl)))
        (let documentation (car ((sxpath '(documentation)) library-group)))
        (for library (in-list ((sxpath '(items structure)) library-group)))
      (let* ((name (cadar ((sxpath '(^ name)) library)))
             (directory (pathname-as-directory
                         (library-name->pathname name output-directory)))
             (html-pathname (pathname-with-file directory "index.html")))
        (fmt #t "Creating HTML documentation for " name "\n")
        (create-directory* (pathname-container html-pathname))
        (call-with-output-file/atomic html-pathname
          (lambda (port)
            (sxml->xml (library-shtml name library documentation) port)))))))

(define (main argv)
  (match argv
    ((program bundle-filename output-directory)
     (guard (c ((error? c)
               (display-condition c (current-error-port))
               (exit 1)))
       (generate-documentation bundle-filename
                               (pathname-as-directory output-directory))
       (exit 0)))))

(main (command-line))

;; Local Variables:
;; scheme-indent-styles: (foof-loop (match 1))
;; End:
