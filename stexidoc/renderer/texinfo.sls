#!r6rs
;; (stexidoc renderer texinfo) -- documenting Scheme as stexinfo

;; Copyright (C) 2009,2010,2011  Free Software Foundation, Inc.
;; Copyright (C) 2003,2004,2009  Andy Wingo <wingo at pobox dot com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;@ Serialization of @code{stexi} to plain texinfo.
(library (stexidoc renderer texinfo)
  (export package-stexi-documentation
          package-stexi-standard-copying
          package-stexi-standard-prologue
          transform-library-listing)
  (import (rnrs)
          (only (srfi :1) append-map concatenate)
          (srfi :8 receive)
          (wak fmt)
          (wak foof-loop)
          (wak ssax tree-trans)
          (spells alist)
          (spells tracing) ;debug
          (spells match)
          (stexidoc util)
          (stexidoc texi))


;;; Package-level procedures

(define (package-stexi-documentation properties get-library)
  (let-properties properties ((name car)
                              (libraries)
                              (texinfo-basename car)
                              (texinfo-epilogue values '()))
    (let ((library-list (concatenate
                         (transform-library-listing
                          libraries
                          (lambda (text) '())
                          values
                          (lambda (lib-name description)
                            lib-name)))))
      `(texinfo
        (% (title ,name)
           (filename ,(string-append texinfo-basename ".info")))
        ,@(package-stexi-standard-prologue properties)
        ,@(package-stexi-extended-menu properties)
        ,@(append-map (lambda (lib-name)
                        (stexi->chapter
                         (receive (overview stdl) (get-library lib-name)
                           (library-stexi-documentation lib-name overview stdl))))
                      library-list)
        ,@texinfo-epilogue))))

(define (package-stexi-standard-prologue properties)
  (let-properties properties ((name car)
                              (version car)
                              (updated car)
                              (authors)
                              (libraries)
                              (texinfo-basename car)
                              (texinfo-category car)
                              (description car))
    `(,(package-stexi-standard-copying properties)
      (dircategory (% (category ,texinfo-category)))
      (direntry
       "* " ,name ": (" ,(string-append texinfo-basename ".info") "). "
       ,description ".")
      ,@(package-stexi-standard-titlepage properties))))

(define (package-stexi-standard-copying properties)
  (let-properties properties ((name car)
                              (version car)
                              (updated car)
                              (years)
                              (copyright-holder)
                              (permissions))
    `(copying
      (para "This manual is for " ,name
            " (version " ,version ", updated " ,updated ")")
      (para "Copyright " ,(fmt #f (fmt-join dsp years ","))
            " " ,@copyright-holder)
      (quotation (%) (para ,@permissions)))))

(define (package-stexi-standard-titlepage properties)
  (let-properties properties ((name car)
                              (version car)
                              (updated car)
                              (authors))
    `((titlepage
       (title ,name)
       (subtitle "version " ,version ", updated " ,updated)
       ,@(map (lambda (pair)
                `(author ,(car pair) " (" (email ,(cdr pair)) ")"))
              authors)
       (page)
       (vskip (% (all "0pt plus 1filll")))
       (insertcopying)))))

(define (package-stexi-extended-menu properties)
  (let-properties properties ((name car)
                              (libraries))
    (package-stexi-generic-menu
     name
     (libraries-stexi-menu libraries))))

(define (package-stexi-generic-menu name menu)
  `((ifnottex
     (node (% (name "Top")))
     (top (% (title ,name)))
     (insertcopying)
     ,menu)
    (iftex
     (shortcontents))))


;;; Library-level procedures

(define (library-stexi-documentation name documentation stdl)
  `(texinfo
    (% (title ,(fmt #f name)))
    (node (% (name ,(library-name->node-name name))))
    ,@(cdr (stdl->stexi `(group (items ,stdl)
                                ,documentation)))))

(define (libraries-stexi-menu libraries)
  (define (make-entry lib-name description)
    (let ((node (library-name->node-name lib-name)))
      `("* " ,node "::"
        ,(make-string (max (- 21 (string-length node)) 2) #\space)
        ,@description "\n")))
  `(menu
    ,@(concatenate (transform-library-listing libraries
                                              (lambda (text)
                                                (list text "\n"))
                                              concatenate
                                              make-entry))))

(define (transform-library-listing libraries
                                   transform-separator
                                   transform-entries
                                   transform-entry)
  (define (flush-entries result entries)
    (if (null? entries)
        result
        (cons (transform-entries (reverse entries)) result)))
  (loop continue ((for entry (in-list libraries))
                  (with result '())
                  (with entries '()))
    => (reverse (flush-entries result entries))
    (cond ((string? entry)
           (continue (=> result (cons (transform-separator entry)
                                      (flush-entries result entries)))
                     (=> entries '())))
          (else
           (let ((texi
                  (match entry
                    ((lib-name . description)
                     (transform-entry lib-name description)))))
             (continue (=> entries (cons texi entries))))))))


;;; stexi helpers

(define (stexi->chapter stexi)
  (pre-post-order
   stexi
   `((texinfo . ,(lambda (tag attrs node . body)
                   `(,node
                     (chapter ,@(assq-ref (cdr attrs) 'title))
                     ,@body)))
     (*TEXT* . ,(lambda (tag text) text))
     (*DEFAULT* . ,(lambda args args)))))

)

;; Local Variables:
;; scheme-indent-styles: ((let-properties 2))
;; End:
