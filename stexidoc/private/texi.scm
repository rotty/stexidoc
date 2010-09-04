;;; texi.scm --- convert stexidoc to plain stexi

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

(define universal-spedl->stexi-rules
  `((^ . ,(lambda (trigger . subs)
            `(% ,@subs)))
    (*fragment* *PREORDER* .
                ,(lambda (tag . spedls)
                   `(*fragment* ,@(append-map
                                   (lambda (spedl) (cdr (spedl->stexi spedl)))
                                   spedls))))
    (*DEFAULT* . ,list)
    (*TEXT* . ,(lambda (trigger text)
                 (cond ((symbol? text)
                        (symbol->string text))
                       (else text))))))
  
(define (spedl->stexi spedl)
  (pre-post-order
   spedl
   `((group
      ((items
        ((structure *MACRO* .
                    ,(lambda (tag attlist . subs)
                       (let ((bindings ((sxpath '(items *)) (cons tag subs)))
                             (interface (assq-ref subs 'interface)))
                         `(structure* ,attlist
                                      ,@(filter-items interface bindings)))))
         (structure* . ,process-structure*)
         (procedure . ,(make-def 'defun 'defunx))
         (name *PREORDER* . ,(lambda (tag name)
                               `(,tag ,(fmt #f name))))
         (arguments *PREORDER* . ,arguments->stexi)
         (variable . ,(make-def 'defvar 'defvarx))
         (syntax . ,(make-def 'defspec 'defspecx))
         ,@universal-spedl->stexi-rules)
        . ,(lambda (tag . procs) procs))
       (documentation *PREORDER* . ,(lambda (tag . docs) docs))
       ,@universal-spedl->stexi-rules)
      . ,process-group)
     ,@universal-spedl->stexi-rules)))


(define (process-group tag items docs)
  (cond ((not (null? items))
         `(*fragment*
           ,@((car items) (append
                           (append-map (lambda (proc) (proc #f)) (cdr items))
                           docs))))
        (else
         '(*fragment*))))

(define (process-structure* tag attlist . items)
  (lambda (to-wrap)
    `((node (% (name ,(fmt #f (attlist-ref attlist 'name)))))
      ,@(append to-wrap (append-map (lambda (item)
                                      (if (eq? '*fragment* (car item))
                                          (cdr item)
                                          item))
                                    items)))))

(define (arguments->stexi tag . args)
  `(arguments
    ,@(fold-right
       (lambda (arg args)
         (match arg
           (('rest-list arg)
            (append! (list "." (symbol->string arg) 
                           args)))
           (('optional . opt-args)
            `("[" ,@(map symbol->string opt-args) "]"
              ,@args))
           (else
            (cons (if (symbol? arg)
                      (symbol->string arg)
                      arg)
                  args))))
       '()
       args)))

(define (make-def type typex)
  (lambda (tag attlist . subs)
    (let ((name (attlist-ref attlist 'name)))
      (lambda (to-wrap)
        (if to-wrap
            `((anchor (% (name ,(fmt #f type "-" name))))
              (,type ,attlist ,@to-wrap))
            `((anchor (% (name ,(fmt #f typex "-" name)))) ;??
              (,typex ,attlist)))))))

(define (filter-items interface items)
  (let ((names (interface-exported-names interface)))
    (define (do-filter tag attlist . subs)
      (let ((name (car (assq-ref (cdr attlist) 'name))))
        (and (memq name names)
             `(,tag ,attlist ,@subs))))
    (pre-post-order
     items
     `((group
        ((items
          ((procedure *PREORDER* . ,do-filter)
           (variable *PREORDER* . ,do-filter)
           (syntax *PREORDER* . ,do-filter))
          . ,(lambda (tag . subs)
               `(items ,@(filter identity subs))))
         (*DEFAULT* *PREORDER* . ,list))
        . ,list)
       (documentation *PREORDER* . ,list)))))

;; arch-tag: be0af3e7-6a35-4b70-9550-6cdbe6a5781c
