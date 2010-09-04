;;; make-doc.sps --- stexidoc command-line interface

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

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
        (spells condition)
        (spells filesys)
        (spells pathname)
        (spells match)
        (spells alist)
        (spells args-fold)
        (spells tracing) ;debug
        (wak foof-loop)
        (wak foof-loop nested)
        (wak fmt)
        (wak ssax tree-trans)
        (wak sxml-tools sxpath)
        (ocelotl ssax-utils)
        (ocelotl wt-tree)
        (dorodango inventory)
        (dorodango package)
        (dorodango bundle)
        (texinfo)
        (texinfo html)
        (stexidoc reader)
        (stexidoc util)
        (stexidoc extract)
        (stexidoc html))


;;; Utilities

(define (die formatter)
  (fmt (current-error-port) (cat "make-doc: " formatter "\n"))
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


;;; Support for dorodango bundles and R6RS libraries

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


;;; Top-level texinfo

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
                  (wrap-html title #f root-path body)))))))


;;; Command-line processing

(define (value-setter name value)
  (lambda (option option-name arg vals)
    (acons name value vals)))

(define options
  (list (option '("bundle") #f (value-setter 'format 'bundle))
        (option '("texi") #f (value-setter 'format 'texi))))

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

(define (main argv)
  (let* ((vals (process-command-line argv))
         (operands (reverse (assq-ref vals 'operands))))
    (case (assq-ref vals 'format)
      ((bundle)
       (match operands
         ((bundle-filename output-directory)
          (guard (c ((error? c)
                     (display-condition c (current-error-port))
                     (exit 1)))
            (generate-documentation bundle-filename
                                    (pathname-as-directory output-directory))
            (exit 0)))
         (_
          (die "expected two arguments"))))
      ((texi)
       (match operands
         ((texi-filename root-path)
          (call-with-input-file-and-directory texi-filename
            (lambda (port)
              (texi->xhtml port (current-output-port) root-path))))
         (_
          (die "expected two arguments")))))))

(main (command-line))

;; Local Variables:
;; scheme-indent-styles: (foof-loop (match 1))
;; End:
