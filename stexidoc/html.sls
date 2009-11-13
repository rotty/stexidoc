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
  (export stdl->shtml libraries-stdl->shtml libraries-stdl->shtml-toc)
  (import (rnrs)
          (only (srfi :1) concatenate)
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
          (stexidoc texi)
          (stexidoc read-r5rs)
          (stexidoc extract))


(define (libraries-stdl->shtml-toc stdl)
  (pre-post-order
   stdl
   `((*fragment*
      ((group ,library-group-toc-rules . ,(lambda (tag . shtml) shtml)))
      . ,(lambda (tag . shtmls)
           `(dl (^ (id "toc"))
                ,@(concatenate shtmls)))))))

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
  `(a (^ (href ,(uri->string (make-uri #f #f '() #f (library-name->id name)))))
      ,(fmt #f name)))

(define (library-name->id name)
  (string-append "lib:" (string-join (map symbol->string name) "_")))


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
