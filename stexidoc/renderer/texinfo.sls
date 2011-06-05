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
          transform-library-listing
          stdl->stexi
          library-identifier->node-name)
  (import (rnrs)
          (only (srfi :1) append! append-map concatenate)
          (srfi :8 receive)
          (only (srfi :13) string-prefix?)
          (wak fmt)
          (wak foof-loop)
          (wak foof-loop nested)
          (wak ssax tree-trans)
          (wak sxml-tools sxpath)
          (spells alist)
          (spells tracing) ;debug
          (spells match)
          (stexidoc util))


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
               `(items ,@(filter values subs))))
         (*DEFAULT* *PREORDER* . ,list))
        . ,list)
       (documentation *PREORDER* . ,list)))))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop (let-properties 2))
;; End:
