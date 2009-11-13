(import (except (rnrs) delete-file file-exists?)
        (only (srfi :13)
              string-join
              string-suffix?)
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
        (ocelotl wt-tree)
        (dorodango inventory)
        (dorodango package)
        (dorodango bundle)
        (stexidoc reader)
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


(define-record-type library-node
  (fields stdl documentation))

(define %library-tag (list 'library-tag))

(define (libraries-stdl->inventory stdl)
  (iterate-values ((inventory (make-inventory 'libraries #f)))
      (for library-group (in-list ((sxpath '(group)) stdl)))
      (let documentation (car ((sxpath '(documentation)) library-group)))
      (for library (in-list ((sxpath '(items structure)) library-group)))
      (let ((name (cadar ((sxpath '(^ name)) library))))
        (inventory-leave-n
         (inventory-update inventory
                           (append (map symbol->string name)
                                   (list %library-tag))
                           #f           ;container?
                           (make-library-node library documentation))
         (+ (length name) 1)))))

(define (inventory->libraries-stdl inventory)
  (loop continue ((with cursor
                        (inventory-cursor inventory)
                        (inventory-cursor-next cursor))
                  (while cursor)
                  (with result '(*fragment*)))
    => result
    (let ((stdl (library-node-stdl
                 (inventory-cursor-data cursor)))
          (documentation (library-node-documentation
                          (inventory-cursor-data cursor))))
      (continue (=> result
                    (merge-fragments `(*fragment* (group (items ,stdl)
                                                         ,documentation))
                                     result))))))

(define (generate-documentation bundle-filename output-directory)
  (define (generate-toc inventory path)
    (let ((html-pathname (pathname-join
                          output-directory
                          (make-pathname #f (reverse path) "index.html"))))
      (fmt #t "Creating TOC for " path " in " (->namestring html-pathname) "\n")
      (create-directory* (pathname-container html-pathname))
      (call-with-output-file/atomic html-pathname
        (lambda (port)
          (sxml->xml (libraries-toc-page (inventory->libraries-stdl inventory)
                                         path)
                     port)))))
  (let ((bundle (open-input-bundle bundle-filename)))
    (iterate! (for package (in-list (bundle-packages bundle)))
        (let inventory (libraries-stdl->inventory
                        (bundle-package->stdl bundle package)))
      (begin
        (let recur ((inventory inventory) (path '()))
          (loop ((for cursor (in-inventory inventory)))
            (when (and (inventory-container? cursor)
                       (not (eq? (inventory-name (inventory-enter cursor))
                                 %library-tag)))
              (let ((path (cons (inventory-name cursor) path)))
                (generate-toc cursor (reverse path))
                (recur cursor path)))))
        (iterate! ((with cursor
                         (inventory-cursor inventory)
                         (inventory-cursor-next cursor))
                   (while cursor))
          (let* ((name (map string->symbol (reverse (inventory-cursor-path cursor))))
                 (node (inventory-cursor-data cursor))
                 (directory (pathname-as-directory
                             (library-name->pathname name output-directory)))
                 (html-pathname (pathname-with-file directory "index.html")))
            (fmt #t "Creating HTML documentation for " name "\n")
            (create-directory* (pathname-container html-pathname))
            (call-with-output-file/atomic html-pathname
              (lambda (port)
                (sxml->xml (library-page name
                                         (library-node-stdl node)
                                         (library-node-documentation node))
                           port)))))))))

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
