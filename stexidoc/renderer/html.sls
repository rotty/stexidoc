#!r6rs
;;; html.sls --- HTML output for stexidoc

;; Copyright (C) 2009-2011 Andreas Rottmann <a.rottmann@gmx.at>
;; Copyright (C) 2006,2007,2009  Andy Wingo <wingo at pobox dot com>

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

(library (stexidoc renderer html)
  (export stdl->shtml
          library-page
          package-toc-page
          wrap-html)
  (import (rnrs)
          (only (srfi :1) concatenate drop make-list)
          (srfi :2 and-let*)
          (srfi :8 receive)
          (only (srfi :13)
                string-join
                string-prefix?
                string-suffix?)
          (srfi :14 char-sets)
          (srfi :39 parameters)
          (wak fmt)
          (wak foof-loop)
          (wak ssax tree-trans)
          (wak sxml-tools sxpath)
          (wak texinfo html)
          (spells alist)
          (spells pathname)
          (spells match)
          (spells condition)
          (spells tracing)
          (only (spells misc) and=> or-map)
          (ocelotl ssax-utils)
          (ocelotl net uri)
          (ocelotl net pct-coding)
          (stexidoc util)
          (stexidoc reader)
          (stexidoc extract)
          (stexidoc renderer texinfo))


(define (library-page package-name name stdl documentation)
  (parameterize ((current-library-reference name))
    (wrap-html (fmt #f name)
               #f
               package-name
               (make-back-filename (length name) #f)
               (list (stdl->shtml `(group (items ,stdl)
                                          ,documentation))))))

(define (package-toc-page properties)
  (let ((title (property-ref properties 'name car)))
    (wrap-html title
               #f
               title
               (make-back-filename 0 #f)
               (list
                (stexi->shtml (package-toc-stexi properties))))))

(define (package-toc-stexi properties)
  `(texinfo
    (% (title "unused"))
    ,@(cdr (package-stexi-standard-copying properties))
    ,@(libraries-toc-stexi (property-ref properties 'libraries))))

(define (libraries-toc-stexi libraries)
  (define (make-entry lib-name description)
    `(entry (% (heading
                (uref (% (url ,(lib-name->ustr lib-name))
                         (title ,(lib-name->str lib-name))))))
            ,@description))
  (transform-library-listing libraries
                             (lambda (text)
                               `(subheading ,text))
                             (lambda (content)
                               `(table (% (formatter (bold))) ,@content))
                             make-entry))

(define (lib-name->str lib-name)
  (fmt #f (dsp lib-name)))
(define (lib-name->ustr lib-name)
  (->namestring (library-name->pathname lib-name '(()))))


(define (libraries-stdl->shtml-toc stdl)
  (pre-post-order
   stdl
   `((*fragment*
      ((group ,library-group-toc-rules
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

(define index-filename "")

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
              (and=> identifier symbol->string)))
  (and-let* ((current (current-library-reference)))
    (receive (library-reference identifier) (parse-node-name node-name)
      (cond ((and library-reference
                  (not identifier))
             (uri->string (make-node-uri current library-reference #f)))
            (identifier
             (let ((node-uri (make-node-uri current
                                            library-reference
                                            identifier)))
               (uri->string
                (if manual-name
                    (merge-uris node-uri (resolve-manual manual-name))
                    node-uri))))
            (else
             #f)))))

(define (shtml-rename-anchor anchor-name)
  (and-let* ((lib-name (current-library-reference))
             (prefix (string-append (library-identifier->node-name lib-name #f)
                                    " "))
             ((string-prefix? prefix anchor-name)))
    (pct-encode (string->utf8 (substring anchor-name
                                         (string-length prefix)
                                         (string-length anchor-name)))
                fragment-safe-charset)))

;; This must correspond to the definition from RFC 3986. The
;; definition is not valid for XHTML 1.1, due to the overly
;; restrictive rules for the `id' attribute, but is valid for (X)HTML
;; 5, and accepted in modern browsers.
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
          ((and (list-of symbol? first) (eof-object? second))
           (values first #f))
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
  ;; We don't know at what URLs other manuals are stored
  (make-parameter '()))

(define (stdl->shtml stdl)
  (parameterize ((stexi-ref-resolvers (cons shtml-resolve-ref
                                            (stexi-ref-resolvers)))
                 (stexi-anchor-renamers (cons shtml-rename-anchor
                                              (stexi-anchor-renamers))))
    (stexi->shtml (stdl->stexi stdl))))

(define library-group-toc-rules
  `((items
     ((structure *PREORDER*
                 . ,(lambda (tag attlist . subs)
                      (library-name->href (attlist-ref attlist 'name)))))
     . ,(lambda (tag . library-names)
          `(dt ,@(list-intersperse library-names " "))))
    (documentation *PREORDER*
                   . ,(lambda (tag . stexi)
                        `(dd ,@(stexi->shtml stexi))))))

(define (library-name->href name)
  (let ((uri-path (append (library-name->path name)
                          (list index-filename))))
    `(a (^ (href ,(uri->string (make-uri #f #f uri-path #f #f))))
        ,(fmt #f name))))


;; HTML template

(define (wrap-html title show-title? heading root-path body)
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
          (div (^ (id "header"))
               ,@(if heading
                     `((h1 (^ (id "heading"))
                           (a (^ (href ,(->namestring root-path))) ,heading)))
                     '()))
          (div (^ (id "inner"))
               ,@(if show-title?
                     `((h2 (^ (class "centered")) ,title))
                     '())
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
;; scheme-indent-styles: (foof-loop (match 1) (let-properties 2))
;; End:
