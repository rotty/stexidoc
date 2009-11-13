;;; read.scm --- A little Scheme reader

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>
;;
;; Based on code from Scheme 48 and scsh; see file COPYING.BSD and
;; AUTHORS for exact license conditions and list of authors.

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; This is a modified version of the Scheme48 reader, including the
;; following changes:
;; - Returns non-forms (e.g. comments) read, as `non-form' objects
;; - Accepts some additional syntax beyond R5RS, such as a
;;   subset of the R6RS additions.
;; - Is case-sensitive

;; Nonstandard things used:
;;  Ascii stuff: char->ascii, ascii->char, ascii-whitespaces, ascii-limit
;;    (for dispatch table; portable definitions in alt/ascii.scm)
;; This is defined in spells.ascii --rotty
;;  reverse-list->string  -- ok to define as follows:
;;    (define (reverse-list->string l n)
;;      (list->string (reverse l)))
;;  make-immutable! -- ok to define as follows:
;;    (define (make-immutable! x) x)
;;  signal (only for use by reading-error; easily excised)

;;; Code:

(define bel (ascii->char 7))
(define bs  (ascii->char  8))
(define ff  (ascii->char 12))
(define cr  (ascii->char 13))
(define ht  (ascii->char  9))
(define vt  (ascii->char 11))


(define (make-immutable! x) x)
(define (reverse-list->string l n)
  (list->string (reverse l)))

(define sml-warn-handler
  (make-parameter (lambda (port msg)
                    (format (current-error-port) "SML WARNING: ~a" msg))))

(define (warn msg port)
  ((sml-warn-handler) port msg))

(define (read-scheme-code port)
  (let loop ((sml '()))
    (let ((form (read-scheme port)))
      (if (eof-object? form)
          (reverse sml)
          (loop (cons form sml))))))

(define (read-scheme port)
  (let loop ()
    (let ((form (sub-read port)))
      (cond ((eof-object? form)          form)
            ((or (non-form? form)
                 (not (reader-token? form)))
             form)
            ((eq? form close-paren)
             ;; Too many right parens.
             (warn "discarding extraneous right parenthesis" port)
             (loop))
            (else
             (reading-error port (cdr form)))))))

(define (sub-read-carefully port)
  (let ((form (sub-read port)))
    (cond ((eof-object? form)
           (reading-error port "unexpected end of file"))
	  ((reader-token? form) (reading-error port (cdr form)))
	  (else form))))

(define reader-token-marker (list 'reader-token))
(define (make-reader-token message) (cons reader-token-marker message))
(define (reader-token? form)
  (and (pair? form) (eq? (car form) reader-token-marker)))

(define-record-type non-form
  (fields type data))

(define close-paren (make-reader-token "unexpected right parenthesis"))
(define dot         (make-reader-token "unexpected \" . \""))


; Main dispatch

(define (sub-read port)
  (let ((c (read-char port)))
    (if (eof-object? c)
        c
        ((vector-ref read-dispatch-vector (char->ascii c))
         c port))))

(define read-dispatch-vector
  (make-vector ascii-limit
               (lambda (c port)
                 (reading-error port "illegal character read" c))))

(define read-terminating?-vector
  (make-vector ascii-limit #t))

(define (set-standard-syntax! char terminating? reader)
  (vector-set! read-dispatch-vector     (char->ascii char) reader)
  (vector-set! read-terminating?-vector (char->ascii char) terminating?))

; Usual read macros

(define (set-standard-read-macro! c terminating? proc)
  (set-standard-syntax! c terminating? proc))

(define (sub-read-list c port)
  (let ((form (sub-read port)))
    (if (eq? form dot)
	(reading-error port
		       "missing car -- ( immediately followed by .")
	(let recur ((form form))
	  (cond ((eof-object? form)
		 (reading-error port
				"end of file inside list -- unbalanced parentheses"))
		((eq? form close-paren) '())
		((eq? form dot)
		 (let* ((last-form (sub-read-carefully port))
			(another-form (sub-read port)))
		   (if (eq? another-form close-paren)
		       last-form
		       (reading-error port
				      "randomness after form after dot"
				      another-form))))
		(else
		 (cons form (recur (sub-read port)))))))))

(define *sharp-macros* '())

(define (define-sharp-macro c proc)
  (set! *sharp-macros* (cons (cons c proc) *sharp-macros*)))

(define (proper-list? x)
  (cond ((null? x) #t)
	((pair? x) (proper-list? (cdr x)))
	(else #f)))


; Tokens

(define (sub-read-token c port)
  (let loop ((l (list c)) (n 1))
    (let ((c (peek-char port)))
      (cond ((or (eof-object? c)
                 (vector-ref read-terminating?-vector (char->ascii c)))
             (reverse-list->string l n))
            (else
	     (read-char port)
             (loop (cons c l)
                   (+ n 1)))))))

(define (parse-token string port)
  (if (let ((c (string-ref string 0)))
	(or (char-numeric? c) (char=? c #\+) (char=? c #\.)
            (and (char=? c #\-)
                 (not (and (>= (string-length string) 2)
                           (char=? (string-ref string 1) #\>))))))
      (cond ((string->number string) => values)
	    ((member string strange-symbol-names)
	     (string->symbol (make-immutable! string)))
	    ((string=? string ".")
	     dot)
	    (else
	     (reading-error port "unsupported number syntax" string)))
      (string->symbol (make-immutable! string))))

(define strange-symbol-names
  '("+" "-" "..."
	"1+" "-1+"  ;Only for S&ICP support
	"->"	    ;Only for JAR's thesis
	))

; Reader errors

(define (reading-error port message . irritants)
  (raise (condition (make-parser-error port)
                    (make-message-condition message)
                    (make-irritants-condition irritants))))



(let ((sub-read-whitespace
       (lambda (c port)
         c                              ;ignored
         (sub-read port))))
  (for-each (lambda (c)
              (vector-set! read-dispatch-vector c sub-read-whitespace))
            ascii-whitespaces))

(let ((sub-read-constituent
       (lambda (c port)
	 (parse-token (sub-read-token c port) port))))
  (for-each (lambda (c)
              (set-standard-syntax! c #f sub-read-constituent))
            (string->list
             (string-append "!$%&*+-./0123456789:<=>?@^_~ABCDEFGHIJKLM"
                            "NOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))))

(set-standard-read-macro! #\( #t sub-read-list)

(set-standard-read-macro! #\) #t
  (lambda (c port)
    c port
    close-paren))

(set-standard-read-macro! #\' #t
  (lambda (c port)
    c
    (list 'quote (sub-read-carefully port))))

(set-standard-read-macro! #\` #t
  (lambda (c port)
    c
    (list 'quasiquote (sub-read-carefully port))))

(set-standard-read-macro! #\, #t
  (lambda (c port)
    c
    (let* ((next (peek-char port))
	   ;; DO NOT beta-reduce!
	   (keyword (cond ((eof-object? next)
			   (reading-error port "end of file after ,"))
			  ((char=? next #\@)
			   (read-char port)
			   'unquote-splicing)
			  (else 'unquote))))
      (list keyword
            (sub-read-carefully port)))))

;;; Code up to and icluding the #\" read macro taken from scsh

;;; Full ANSI C strings:
;;; - read as themselves: \\ \? \" \'
;;; - control chars:
;;;   \a alert (bell -- ^g)
;;;   \b backspace (^h)
;;;   \f form feed (^l)
;;;   \n newline (^j)
;;;   \r carriage return (^m)
;;;   \t tab (^i)
;;;   \v vertical tab (^k)
;;; - octal escapes \nnn
;;; - hex escapes \xnn

;;; Is this the elegant thing to do? Too much might make it hard to shift
;;; to Unicode implementations. How about \^g for embedding control chars?
;;; And I haven't done anything about chars (as opposed to strings).

(set-standard-read-macro! #\" #t
  (lambda (c port)
    c ;ignored
    (let* ((readc (lambda ()
		    (let ((c (read-char port)))
		      (if (eof-object? c)
			  (reading-error port "end of file within a string")
			  c))))
	   (read-digit (lambda (base base-name)
			 (let* ((c (readc))
				(d (- (char->ascii c) (char->ascii #\0))))
			   (if (and (<= 0 d) (< d base)) d
			       (reading-error port
					      (string-append "invalid "
							     base-name
							     " code in string.")
					      d))))))

      (let loop ((l '()) (i 0))
	(let ((c (readc)))
	  (cond ((char=? c #\\)
		 (let* ((c (readc))
			(rc (case c
			      ((#\\ #\" #\? #\') c)
			      ((#\a) bel)
			      ((#\b) bs)
			      ((#\f) ff)
			      ((#\n) #\newline)
			      ((#\r) cr)
			      ((#\t) ht)
			      ((#\v) vt)
			      ((#\0 #\1 #\2 #\3)
			       (let* ((d1 (- (char->ascii c) (char->ascii #\0)))
				      (d2 (read-digit 8 "octal"))
				      (d3 (read-digit 8 "octal")))
				 (ascii->char (+ (* 64 d1) (+ (* 8 d2) d3)))))
			      ((#\x)
			       (let ((d1 (read-digit 16 "hex"))
				     (d2 (read-digit 16 "hex")))
				 (ascii->char (+ (* 16 d1) d2))))
			      (else
			       (reading-error port
					      "invalid escapedcharacter in string"
					      c)))))
		   (loop (cons rc l) (+ i 1))))
		((char=? c #\")
		 (reverse-list->string l i))
		(else
		 (loop (cons c l) (+ i 1)))))))))

(set-standard-read-macro! #\; #t
  (lambda (c port)
    (let ((line (read-line port)))
      (if (eof-object? line)
          (make-non-form 'comment "")
          (make-non-form 'comment line)))))

(set-standard-read-macro! #\# #f
  (lambda (c port)
    c ;ignored
    (let* ((c (peek-char port))
	   (c (if (eof-object? c)
		  (reading-error port "end of file after #")
		  (char-downcase c)))
	   (probe (assq c *sharp-macros*)))
      (if probe
	  ((cdr probe) c port)
	  (reading-error port "unknown # syntax" c)))))

(define-sharp-macro #\f
  (lambda (c port) (read-char port) #f))

(define-sharp-macro #\t
  (lambda (c port) (read-char port) #t))

(define-sharp-macro #\\
  (lambda (c port)
    (read-char port)
    (let ((c (peek-char port)))
      (cond ((eof-object? c)
	     (reading-error port "end of file after #\\"))
	    ((char-alphabetic? c)
	     (let ((name (sub-read-carefully port)))
	       (cond ((= (string-length (symbol->string name)) 1)
		      c)
		     ((assq name '((space   #\space)
				   (newline #\newline)
                                   (tab #\t)
                                   (return #\r)))
		      => cadr)
		     (else
		      (reading-error port "unknown #\\ name" name)))))
	    (else
	     (read-char port)
	     c)))))

(define-sharp-macro #\(
  (lambda (c port)
    (read-char port)
    (let ((elts (sub-read-list c port)))
      (if (proper-list? elts)
	  (list->vector elts)
	  (reading-error port "dot in #(...)")))))

(define-sharp-macro #\!
  (lambda (c port)
    (read-char port)
    (let ((token (sub-read-token (read-char port) port)))
      (make-non-form 'directive (string->symbol token)))))

(define-sharp-macro #\'
  (lambda (c port)
    (read-char port)
    (list 'syntax (sub-read-carefully port))))

(define-sharp-macro #\`
  (lambda (c port)
    (read-char port)
    (list 'quasisyntax (sub-read-carefully port))))

(define-sharp-macro #\,
  (lambda (c port)
    (read-char port)
    (list 'unsyntax (sub-read-carefully port))))

(define-sharp-macro #\|
  (lambda (c port)
    (define (assert-not-eof c)
      (when (eof-object? c)
        (reading-error port "unexpected end of file in #| comment")))
    (read-char port)
    (let loop ((comment '()))
      (let ((c (read-char port)))
        (assert-not-eof c)
        (cond ((char=? c #\|)
               (let ((c2 (read-char port)))
                 (assert-not-eof c2)
                 (if (char=? c2 #\#)
                     (make-non-form 'comment (list->string (reverse comment)))
                     (loop (cons c2 (cons c comment))))))
              (else
               (loop (cons c comment))))))))

(define-sharp-macro #\;
  (lambda (c port)
    (read-char port)
    (make-non-form 'comment (sub-read-carefully port))))

(let ((number-sharp-macro
       (lambda (c port)
	 (let ((string (sub-read-token #\# port)))
	   (or (string->number string)
	       (reading-error port "unsupported number syntax" string))))))
  (for-each (lambda (c)
	      (define-sharp-macro c number-sharp-macro))
	    '(#\b #\o #\d #\x #\i #\e)))


;; Local Variables:
;; scheme-indent-styles: ((set-standard-read-macro! 2))
;; End:

