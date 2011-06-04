;;; texi.scm --- convert stexidoc to plain stexi

;; Copyright (C) 2009, 2011 Andreas Rottmann <a.rottmann@gmx.at>

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

(define (stdl->stexi stdl)
  (pre-post-order
   stdl
   `((group
      ((items
        ((structure *MACRO* .
                    ,(lambda (tag attlist . subs)
                       (let ((bindings ((sxpath '(items *)) (cons tag subs)))
                             (interface (assq-ref subs 'interface)))
                         (process-structure attlist interface bindings))))
         (structure* *PREORDER* . ,process-structure*)
         ,@(make-item-rules #f))
        . ,(lambda (tag . procs) procs))
       (documentation *PREORDER* . ,(lambda (tag . docs) docs))
       ,@(make-universal-stdl->stexi-rules))
      . ,process-group)
     ,@(make-universal-stdl->stexi-rules))))

(define (make-universal-stdl->stexi-rules)
  `((^
     ((name *PREORDER* . ,(lambda (tag name)
                            `(,tag ,(fmt #f name))))
      (arguments *PREORDER* . ,arguments->stexi))
     . ,(lambda (tag . subs)
          `(% ,@subs)))
    (*fragment* *PREORDER* .
                ,(lambda (tag . stdls)
                   `(*fragment* ,@(append-map
                                   (lambda (stdl) (cdr (stdl->stexi stdl)))
                                   stdls))))
    (*DEFAULT* . ,list)
    (*TEXT* . ,(lambda (trigger text)
                 (cond ((symbol? text)
                        (symbol->string text))
                       (else text))))))


(define (make-item-rules structure-name)
  `((procedure . ,(make-def 'defun 'defunx structure-name))
    (variable . ,(make-def 'defvar 'defvarx structure-name))
    (syntax . ,(make-def 'defspec 'defspecx structure-name))
    ,@(make-universal-stdl->stexi-rules)))

;; Here we apply the procedures accepting `to-wrap' arguments
;; constructed during stylesheet application; this way we handle
;; `def*x' commands.
(define (process-group tag items docs)
  (cond ((not (null? items))
         `(*fragment*
           ,@((car items) (append
                           (append-map (lambda (proc) (proc #f)) (cdr items))
                           docs))))
        (else
         '(*fragment*))))

(define (process-structure attlist interface bindings)
  (let* ((name (attlist-ref attlist 'name))
         (documentation (documentation-processor name interface)))
    (pre-post-order
     `(structure* ,(lambda () attlist) ,@(filter-items interface bindings))
     `((group
        ((items
          (,@(make-item-rules name)) . ,(lambda (tag . procs) procs))
         (documentation *PREORDER* . ,documentation)
         ,@(make-universal-stdl->stexi-rules))
        . ,process-group)
       (documentation *PREORDER* . ,documentation)
       ,@(make-universal-stdl->stexi-rules)))))

(define (documentation-processor structure-name interface)
  (let ((names (interface-exported-names interface)))
    (define (transform-ref tag args)
      (let ((node (attlist-ref args 'node)))
        (if (memq (string->symbol node) names)
            `(ref (% (node ,(library-identifier->node-name structure-name node))
                     (section ,node)))
            (list tag args))))
    (define (transform-anchor tag args)
      ;; This is a hack to unify the auto-generated anchors by the
      ;; texinfo parser with what we want.
      (let* ((name (attlist-ref args 'name))
             (name-len (string-length name)))
        (loop continue ((for command (in-list definition-commands)))
          => (list tag args)
          (let* ((command-string (symbol->string command))
                 (command-len (string-length command-string)))
            (cond ((and (> name-len command-len)
                        (string-prefix? command-string name)
                        (char=? #\- (string-ref name command-len))
                        (memq (string->symbol
                               (substring name (+ command-len 1) name-len))
                              names))
                   => (lambda (entry)
                        `(anchor
                          (% (name ,(library-identifier->node-name
                                     structure-name
                                     (car entry)))))))
                  (else
                   (continue)))))))
    (lambda stexi
      (cdr (pre-post-order
            stexi
            `((ref *PREORDER* . ,transform-ref)
              (anchor *PREORDER* . ,transform-anchor)
              (*TEXT* . ,(lambda (tag x) x))
              (*DEFAULT* . ,list)))))))

(define definition-commands
  '(deftp defcv defivar deftypeivar defop deftypeop defmethod
    deftypemethod defopt defvr defvar deftypevr deftypevar deffn
    deftypefn defspec defmac defun deftypefun))

(define (process-structure* tag get-attlist . items)
  (let ((attlist (get-attlist)))
    (lambda (to-wrap)
      `((node (% (name ,(library-identifier->node-name
                         (attlist-ref attlist 'name)
                         #f))))
        (section ,(fmt #f  (attlist-ref attlist 'name)))
        ,@(append to-wrap (append-map (lambda (item)
                                        (if (eq? '*fragment* (car item))
                                            (cdr item)
                                            item))
                                      items))))))

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

(define (make-def type typex structure-name)
  (lambda (tag attlist . subs)
    (let ((name (library-identifier->node-name structure-name
                                               (attlist-ref attlist 'name))))
      (lambda (to-wrap)
        (if to-wrap
            `((anchor (% (name ,name)))
              (,type ,attlist ,@(filter-anchors name to-wrap)))
            `((anchor (% (name ,name))) ;++should do this only when name differs
              (,typex ,attlist)))))))

(define (filter-anchors name items)
  (filter (lambda (item)
            (match item
              (('anchor ('% ('name anchor-name)))
               (not (string=? name anchor-name)))
              (else
               'no-match)))
          items))

(define (library-identifier->node-name structure-name item-name)
  (let ((s (cond ((and structure-name item-name)
                  (fmt #f structure-name " " item-name))
                 (item-name
                  (fmt #f item-name))
                 (else
                  (fmt #f structure-name)))))
    (collect-string (for c (in-string s))
        (if (not (memv c '(#\{ #\} #\@ #\, #\.))))
      c)))

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

;; Local Variables:
;; scheme-indent-styles: (foof-loop)
;; End:

