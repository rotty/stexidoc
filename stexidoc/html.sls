;;; html.sls --- HTML output for stexidoc

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
#!r6rs

(library (stexidoc html)
  (export stdl->shtml
          library-page
          libraries-toc-page
          wrap-html)
  (import (rnrs)
          (only (srfi :1) concatenate drop make-list)
          (srfi :8 receive)
          (only (srfi :13)
                string-join
                string-prefix?
                string-suffix?)
          (srfi :14 char-sets)
          (srfi :39 parameters)
          (spells alist)
          (wak foof-loop)
          (spells pathname)
          (wak fmt)
          (spells match)
          (spells condition)
          (spells tracing)
          (only (spells misc) or-map)
          (xitomatl ssax tree-trans)
          (xitomatl sxml-tools sxpath)
          (ocelotl ssax-utils)
          (ocelotl net uri)
          (ocelotl net pct-coding)
          (texinfo html)
          (stexidoc util)
          (stexidoc texi)
          (stexidoc reader)
          (stexidoc extract))


(define (library-page name library documentation)
  (parameterize ((current-library-reference name))
    (wrap-html (fmt #f name)
               `(span "(" ,@(back-href-list name) ")")
               (make-back-filename (length name) #f)
               (list (stdl->shtml `(group (items ,library)
                                          ,documentation))))))

(define (libraries-toc-page stdl path)
  (wrap-html (fmt #f (fmt-join dsp path " ") toc-heading-suffix)
             `(span ,@(back-href-list path) ,toc-heading-suffix)
             (make-back-filename (length path) #f)
             (list
              (libraries-stdl->shtml-toc stdl (length path)))))

(define toc-heading-suffix " - Table of contents")


(define (libraries-stdl->shtml-toc stdl n-strip)
  (pre-post-order
   stdl
   `((*fragment*
      ((group ,(make-library-group-toc-rules n-strip)
              . ,(lambda (tag . shtml) shtml)))
      . ,(lambda (tag . shtmls)
           `(dl (^ (id "toc"))
                ,@(concatenate shtmls)))))))

#;
(define (libraries-stdl->shtml stdl)
  (pre-post-order
   stdl
   `((*fragment*
      ((group *PREORDER*
              . ,(lambda group
                   (let* ((attlist (car ((sxpath '(items structure ^)) group)))
                          (name (attlist-ref attlist 'name)))
                     `((h1 (^ (id ,(library-name->id name)))
                           (a (^ (href "#toc")) ,(fmt #f name)))
                       ,(stdl->shtml group))))))
      . ,(lambda (tag . shtmls)
           `(div ,@(concatenate shtmls)))))))

(define index-filename "index.html")

(define (shtml-resolve-ref node-name manual-name)
  (define (make-node-uri current library-reference identifier)
    (make-uri #f
              #f
              (if library-reference
                  (append (make-list (length current) "..")
                          (map symbol->string library-reference)
                          (list index-filename))
                  '())
              #f
              (symbol->string identifier)))
  (receive (library-reference identifier) (parse-node-name node-name)
    (cond ((and identifier
                (or (not library-reference)
                    (current-library-reference)))
           => (lambda (current)
                (let ((node-uri (make-node-uri current
                                               library-reference
                                               identifier)))
                  (uri->string
                   (if manual-name
                       (merge-uris node-uri (resolve-manual manual-name))
                       node-uri)))))
          (else
           #f))))

(define (shtml-rename-anchor anchor-name)
  (or-map
   (lambda (prefix)
     (and (string-prefix? prefix anchor-name)
          (pct-encode (string->utf8 (substring anchor-name
                                               (string-length prefix)
                                               (string-length anchor-name)))
                      fragment-safe-charset)))
   '("defvar-" "defvarx-"
     "defun-" "defunx-"
     "defspec-" "defspecx-")))

;; This must correspond to the definition from RFC 3986. The
;; definition is not valid for XHTML 1.1, due to the overly strictive
;; rules for the `id' attribute, but is valid for (X)HTML 5, and
;; accepted in modern browsers.
(define fragment-safe-charset
  (char-set-union char-set:letter+digit
                  (string->char-set "-._~!$&'()*+,;=/?:@")))

(define (resolve-manual manual-name)
  (loop continue ((for resolver (in-list (stexidoc-shtml-manual-resolvers))))
    => (error 'resolve-manual "could not resolve manual reference" manual-name)
    (or (resolver manual-name)
        (continue))))

(define (parse-node-name node-name)
  (let* ((port (open-string-input-port node-name))
         (first (read port))
         (second (read port))
         (third (read port)))
    (cond ((and (list-of symbol? first) (symbol? second) (eof-object? third))
           (values first second))
          ((and (symbol? first) (eof-object? second))
           (values #f first))
          (else
           (values #f #f)))))

(define (list-of predicate thing)
  (and (pair? thing)
       (predicate (car thing))
       (let ((rest (cdr thing)))
         (or (null? rest)
             (list-of predicate rest)))))

(define current-library-reference
  (make-parameter #f))

(define stexidoc-shtml-manual-resolvers
  ;; We don't at what URLs other manuals are stored
  (make-parameter '()))

(define (stdl->shtml stdl)
  (parameterize ((stexi-ref-resolvers (cons shtml-resolve-ref
                                            (stexi-ref-resolvers)))
                 (stexi-anchor-renamers (cons shtml-rename-anchor
                                              (stexi-anchor-renamers))))
    (stexi->shtml (spedl->stexi stdl))))

(define (make-library-group-toc-rules n-strip)
  `((items
     ((structure *PREORDER*
                 . ,(lambda (tag attlist . subs)
                      (library-name->href (attlist-ref attlist 'name) n-strip))))
     . ,(lambda (tag . library-names)
          `(dt ,@(list-intersperse library-names " "))))
    (documentation *PREORDER*
                   . ,(lambda (tag . stexi)
                        `(dd ,@(stexi->shtml stexi))))))

(define (library-name->href name n-strip)
  (let ((uri-path (append (library-name->path (drop name n-strip))
                          (list index-filename))))
    `(a (^ (href ,(uri->string (make-uri #f #f uri-path #f #f))))
        ,(fmt #f name))))


;; HTML template

(define (wrap-html title heading root-path body)
  `(html (^ (xmlns "http://www.w3.org/1999/xhtml"))
    (head
     (title ,title)
     (meta (^ (name "Generator")
              (content "STexiDoc, a Scheme documentation extractor")))
     (style (^ (type "text/css") (media "screen"))
       "@import url("
       ,(->namestring (pathname-with-file root-path "screen.css"))
       ");"))
    (body
     (div (^ (id "content"))
          ,@(if heading
                '((h1 (^ (id "heading")) ,heading))
                '())
          (div (^ (id "inner"))
               ,@body)
          (div (^ (id "footer"))
               "powered by "
               (a (^ (href ,stexidoc-homepage-url)) "stexidoc"))))))

(define stexidoc-homepage-url "http://github.com/rotty/stexidoc")

(define (back-href-list path)
  (let ((n-parts (length path)))
    (loop ((for i (down-from n-parts (to 0)))
           (for part (in-list path))
           (for result (appending
                        (if (= i 0)
                            (list part)
                            `((a (^ (href ,(make-back-filename i index-filename)))
                                 ,part)
                              " ")))))
      => result)))


(define (make-back-filename n file)
  (string-append
   (if (= n 0)
       "."
       (string-join (make-list n "..") "/" 'suffix))
   (or file "")))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop (match 1))
;; End:
