;;; html.sls --- HTML output for stexidoc

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

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
  (export library-page libraries-toc-page)
  (import (rnrs)
          (only (srfi :1) concatenate drop make-list)
          (only (srfi :13) string-suffix? string-join)
          (spells alist)
          (spells foof-loop)
          (spells pathname)
          (spells fmt)
          (spells match)
          (spells condition)
          (xitomatl ssax tree-trans)
          (xitomatl sxml-tools sxpath)
          (xitomatl ssax extras)
          (ocelotl net uri)
          (texinfo html)
          (stexidoc util)
          (stexidoc texi)
          (stexidoc read-r5rs)
          (stexidoc extract))


(define (library-page name library documentation)
  (wrap-html (fmt #f name)
             `(span "(" ,@(back-href-list name) ")")
             (make-back-filename (length name) #f)
             (list (stdl->shtml `(group (items ,library)
                                        ,documentation)))))

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

(define (stdl->shtml stdl)
  (stexi->shtml (spedl->stexi stdl)))

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
                          (list "index.html"))))
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

(define stexidoc-homepage-url "http://github.com/rotty/stexidoc")

(define (back-href-list path)
  (let ((n-parts (length path)))
    (loop ((for i (down-from n-parts (to 0)))
           (for part (in-list path))
           (for result (appending
                        (if (= i 0)
                            (list part)
                            `((a (^ (href ,(make-back-filename i "index.html")))
                                 ,part)
                              " ")))))
      => result)))


(define (make-back-filename n file)
  (string-append
   (if (= n 0)
       "."
       (string-join (make-list n "..") "/" 'suffix))
   (or file "")))

(define (attlist-ref attlist key)
  (car (assq-ref (cdr attlist) key)))

(define (list-intersperse src-l elem)
  (if (null? src-l) src-l
    (let loop ((l (cdr src-l)) (dest (cons (car src-l) '())))
      (if (null? l) (reverse dest)
        (loop (cdr l) (cons (car l) (cons elem dest)))))))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop (match 1))
;; End:
