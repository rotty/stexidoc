;;; make-doc.sps --- stexidoc command-line ;; Copyright (C) 2009-2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(import (except (rnrs) delete-file file-exists?)
        (only (srfi :1) make-list)
        (srfi :2 and-let*)
        (only (srfi :13)
              string-join
              string-suffix?)
        (srfi :39 parameters)
        (only (spells misc) and=> unspecific)
        (only (spells opt-args) define*)
        (spells condition)
        (spells filesys)
        (spells pathname)
        (spells match)
        (spells alist)
        (spells args-fold)
        (spells tracing) ;debug
        (spells logging)
        (wak foof-loop)
        (wak foof-loop nested)
        (wak fmt)
        (wak ssax tree-trans)
        (wak sxml-tools sxpath)
        (wak texinfo)
        (wak texinfo html)
        (wak texinfo serialize)
        (ocelotl ssax-utils)
        (ocelotl wt-tree)
        (dorodango inventory)
        (dorodango package)
        (dorodango bundle)
        (stexidoc reader)
        (stexidoc util)
        (stexidoc extract)
        (stexidoc renderer texinfo)        
        (stexidoc renderer html))


;;; Utilities

(define (die . formatters)
  (fmt (current-error-port) (cat "make-doc: " (apply-cat formatters) "\n"))
  (exit #f))

(define (sxpath-ref sxml path)
  (and-let* ((result ((sxpath path) sxml))
             ((pair? result)))
    (car result)))

(define sxpath-ref*
  (case-lambda
    ((sxml path convert)
     (match ((sxpath path) sxml)
       (((name (? string? value))) (convert value))
       (_                       #f)))
    ((sxml path)
     (sxpath-ref* sxml path values))))

(define (arg-ref key %-args)
  (and=> (assq key (cdr %-args)) cdr))

(define (arg-req key %-args)
  (or (arg-ref key %-args)
      (assertion-violation "Missing argument" key %-args)))


;;; Support for dorodango bundles and R6RS libraries

(define (bundle-packages->stdl bundle packages)
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
  (iterate-values ((stdl '(*fragment*)))
      => (cons '*fragment* (reverse (cdr stdl)))
      (for package (in-list packages))
      (for pathname extractor (in-bundle-inventory
                               bundle
                               (package-category-inventory package
                                                           'libraries)))
    (if (appealing-pathname? pathname)
        (merge-fragments (read-file pathname extractor) stdl)
        stdl)))

(define (appealing-pathname? pathname)
  (let ((filename (file-namestring pathname)))
    (and (string-suffix? ".sls" filename)
         (not (exists (lambda (implementation)
                        (string-suffix? (string-append "." implementation ".sls")
                                        filename))
                      r6rs-implementations)))))

(define r6rs-implementations
  '("ikarus" "ypsilon" "larceny" "mzscheme" "mosh" "guile"))

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

(define (generate-documentation bundle-filename renderer)
  (call-with-input-bundle bundle-filename
    (lambda (bundle)
      (iterate! (for entry (in-list (bundle-package-map bundle)))
        (match entry
          ((path . packages)
           (generate-package-documentation bundle
                                           packages
                                           renderer)))))))

(define (generate-package-documentation bundle packages renderer)
  (let ((inventory (libraries-stdl->inventory
                    (bundle-packages->stdl bundle packages))))
    (iterate! (for package (in-list packages))
        (for doc-file (in-list (package-property package 'stexidoc '())))
      (begin
        (fmt #t "Processing " doc-file "\n")
        (let ((properties (collect-list (for datum (in-file doc-file read))
                            datum)))
          (renderer properties inventory))))))


;;; An inventory for libraries

(define-record-type library-node
  (fields stdl documentation))

(define %library-tag (list 'library-tag))

(define (libraries-stdl->inventory stdl)
  (iterate-values ((inventory (make-inventory 'libraries #f)))
      (for library-group (in-list ((sxpath '(group)) stdl)))
      (let documentation (sxpath-ref library-group '(documentation)))
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


(define (lookup-library-node inventory lib-name)
  (and=> (inventory-ref inventory (map symbol->string lib-name))
         (lambda (cursor)
           (and (inventory-container? cursor)
                (loop continue ((for child (in-inventory cursor)))
                  => #f
                  (if (eq? %library-tag (inventory-name child))
                      (inventory-data child)
                      (continue)))))))

(define (make-library-getter inventory)
  (lambda (lib-name)
    (and=> (lookup-library-node inventory lib-name)
           (lambda (lib)
             (values (library-node-documentation lib)
                     (library-node-stdl lib))))))

;;; XHTML rendering

(define (make-xhtml-renderer output-pathname)
  (let ((output-directory (pathname-as-directory output-pathname)))
    (lambda (properties inventory)
      (let-properties properties ((name car)
                                  (libraries))
        (generate-package-xhtml properties inventory output-directory)
        (iterate! (for entry (in-list libraries))
          (match entry
            ((lib-name . description)
             (let ((node (lookup-library-node inventory lib-name)))
               (unless node
                 (die "reference to unknown library: " lib-name))
               (generate-library-xhtml name lib-name node output-directory)))
            (_
             (unspecific))))))))

(define (generate-library-xhtml package-name name node output-directory)
  (let* ((directory (pathname-as-directory
                     (library-name->pathname name output-directory)))
         (html-pathname (pathname-with-file directory "index.html")))
    (fmt #t "Creating HTML documentation for " name "\n")
    (create-directory* (pathname-container html-pathname))
    (call-with-output-file/atomic html-pathname
      (lambda (port)
        (sxml->xml (library-page package-name
                                 name
                                 (library-node-stdl node)
                                 (library-node-documentation node))
                   port)))))

(define (generate-package-xhtml properties inventory output-directory)
  (call-with-output-file/atomic (merge-pathnames "index.html" output-directory)
    (lambda (port)
      (sxml->xml (package-toc-page properties) port))))


;;; Texinfo rendering

(define (make-texinfo-renderer output-pathname)
  (let ((output-directory (pathname-as-directory output-pathname)))
    (lambda (properties inventory)
      (let-properties properties ((texinfo-basename car))
        (call-with-output-file/atomic
          (merge-pathnames (make-pathname #f '() (make-file texinfo-basename "texi"))
                           output-directory)
          (lambda (port)
            (put-string port
                        (stexi->texi (package-stexi-documentation
                                      properties
                                      (make-library-getter inventory))))))))))


;;; Render texinfo into XHTML

(define (texi->xhtml texi-port xhtml-port root-path)
  (let ((stexi (texi->stexi texi-port)))
    #;
    (fmt #t "STEXI: " (pretty/unshared stexi))
    (sxml->xml (adorn-shtml (stexi->shtml stexi) root-path) xhtml-port)))

(define (adorn-shtml shtml root-path)
  (pre-post-order
   shtml
   `((html *PREORDER*
           . ,(lambda shtml
                (let ((title (sxpath-ref* shtml '(head title)))
                      (body ((sxpath '(body)) shtml)))
                  #;
                  (fmt #t (pretty (list title body)))
                  (wrap-html title #f #f root-path body)))))))


;;; Command-line processing

;; command-line syntax:
;;
;;   make-doc -i texi|bundle -o texi|html INPUT OUTPUT

(define (value-setter name value)
  (lambda (option option-name arg vals)
    (acons name value vals)))

(define arg-setter
  (case-lambda
    ((name convert)
     (lambda (option option-name arg vals)
       (acons name (convert arg) vals)))
    ((name)
     (arg-setter name values))))

(define options
  (list (option '(#\i "--from") 'input-format
                (arg-setter 'input-format string->symbol))
        (option '(#\o "--to") 'output-format
                (arg-setter 'output-format string->symbol))))

(define (process-command-line argv)
  (define (unrecognized-option option name arg vals)
    (die (cat "unknown option: " name)))
  (define (process-operand operand vals)
    (apush 'operands operand vals))
  (args-fold (cdr argv)
             options
             unrecognized-option
             process-operand
             '((operands . ())
               (format . bundle))))

(define (dsp-error c)
  (cond ((and (stacked-condition? c)
              (message-condition? c))
         (cat (condition-message c) nl
              (dsp-error (next-condition c))))
        (else
         (dsp-condition c))))

(define (make-renderer format output-directory)
  (case format
    ((texi) (make-texinfo-renderer output-directory))
    ((xhtml) (make-xhtml-renderer output-directory))
    (else
     (die "unsupported output format: " format))))

(define (main argv)
  (let* ((vals (process-command-line argv))
         (operands (reverse (assq-ref vals 'operands)))
         (input-format (assq-ref vals 'input-format))
         (output-format (assq-ref vals 'output-format)))
    (let-logger-properties
        ((root-logger
          `((threshold info)
            (handlers
             ,(lambda (entry)
                (default-log-formatter entry (current-output-port)))))))
      (case input-format
        ((bundle)
         (match operands
           ((bundle-filename output-directory)
            (guard (c ((error? c)
                       (fmt (current-error-port) (dsp-error c))
                       (exit 1)))
              (generate-documentation bundle-filename
                                      (make-renderer output-format
                                                     output-directory))))
           (_
            (die "expected two arguments"))))
        ((texi)
         (unless (eq? 'html output-format)
           (die "non-HTML output for texinfo not currently supported"))
         (match operands
           ((texi-filename root-path)
            (call-with-input-file-and-directory texi-filename
              (lambda (port)
                (texi->xhtml port (current-output-port) root-path))))
           (_
            (die "expected two arguments"))))))))

(main (command-line))

;; Local Variables:
;; scheme-indent-styles: (foof-loop (match 1) (let-logger-properties 1)
;;                        (let-properties 2) (with-texi-commands 1))
;; End:
