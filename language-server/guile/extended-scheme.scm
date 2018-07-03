;;;; Copyright (C) 2018 Jonas Herzig <me@johni0702.de>
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;;;

(define-module (language-server guile extended-scheme)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)

  #:export (read-expression
            read-all
            string->escm
            string->escm-list
            escm->scm))

(define (string->escm-list str)
  (call-with-input-string str (lambda (port) (read-all port))))

(define (string->escm str)
  (call-with-input-string str (lambda (port) (read-expression port))))

(define (port-src port)
  `((filename . ,(port-filename port))
    (line . ,(port-line port))
    (column . ,(port-column port))))

(define (must-not-be-eof port where x)
  (if (eof-object? x) (read-error port where "end of file"))
  x)
(define (read-error port where what)
  (let ((message (string-append (or (port-filename port) "#<unknown port>")
                                ":" (number->string (+ 1 (port-line port)))
                                ":" (number->string (+ 1 (port-column port)))
                                ": " what)))
    (throw 'read-error where message '() #f)))

(define (make-escm src type content) (cons* type src content))

(define (make-escm* src type content)
  (make-escm (append (car content) src) type (cdr content)))

(define whitespace '(#\newline #\space #\tab #\return))

(define (read-whitespace port)
  "Consume any leading whitespace."
  (let lp ()
    (let ((char (get-char port)))
      (if (member char whitespace)
        (lp)
        (unless (eof-object? char) (unget-char port char))))))

(define (read-sexp port)
  "Read multiple expressions contained within parenthesis."
  (let lp ((acc '()))
    (read-whitespace port)
    (if (eq? #\) (lookahead-char port))
      (begin
        (get-char port)
        (reverse acc))
      (lp (cons (must-not-be-eof port "scm_i_lreadparen" (read-expression port))
                acc)))))

(define (read-special src type port)
  "Read special syntax for quote, quasiquote and unquote."
  (make-escm src type (read-expression port)))

(define (read-comment port)
  "Read the rest of the current line as a comment."
  (let* ((line-or-eof (get-line port))
         (line (if (eof-object? line-or-eof) "" line-or-eof)))
    line))

(define (read-native port)
  "Read from the port by calling `read`.
Return pair of raw data read in form of `((raw . ,data)) and result of `read`."
  (define raw-data '())
  (define done #f)
  (define soft-port
    (make-soft-port
     (vector #f
             #f
             #f
             (lambda ()
               (if done
                 #f
                 (let ((char (get-char port)))
                   (unless (eof-object? char)
                     (set! raw-data (cons char raw-data)))
                   char)))
             (lambda ()
               ;; Don't close port because we might need to unget-char on it
               #f))
     "r"))
  (set-port-filename! soft-port (port-filename port))
  (set-port-column! soft-port (port-column port))
  (set-port-line! soft-port (port-line port))
  (set-port-encoding! soft-port (port-encoding port))
  (let ((result (read soft-port)))
    (set! done #t)
    ;; Drain everything which has been put into soft-port by unget-* and
    ;; unget it back into port.
    (let ((drained-chars
           (let lp ((acc 0))
             (let ((char (get-char soft-port)))
               (if (eof-object? char)
                 acc
                 (begin
                   (unget-char port char)
                   (lp (+ 1 acc))))))))
      (cons (acons 'raw
                   (string-drop-right (string-reverse (list->string raw-data))
                                      drained-chars)
                   '())
            result))))

(define (read-string port)
  "Read a string."
  (unget-char port #\")
  (read-native port))

(define (read-hash port)
  "Read using a procedure installed with read-hash-extend."
  (unget-char port #\#)
  (read-native port))

(define (read-symbol port)
  "Read a symbol."
  (read-native port))

(define (read-expression port)
  "Read a single scheme expression."
  (read-whitespace port)
  (let ((src (port-src port)))
    (match (get-char port)
      (#\; (make-escm src 'comment (read-comment port)))
      (#\( (make-escm src 'sexp (read-sexp port)))
      (#\) (read-error port "read_inner_expression" "unexpected \")\""))
      (#\' (read-special src 'quote port))
      (#\` (read-special src 'quasiquote port))
      (#\, (if (eq? (lookahead-char port) #\@)
             (begin
               (get-char port)
               (read-special src 'unquote-splicing port))
             (read-special src 'unquote port)))
      (#\" (make-escm* src 'string (read-string port)))
      (#\# (make-escm* src 'hash (read-hash port)))
      (#\page (make-escm src 'page #f))
      (x (if (eof-object? x)
           x
           (begin
             (unget-char port x)
             (make-escm* src 'sym (read-symbol port))))))))

(define (read-all port)
  "Read all expressions from port."
  (let lp ((acc '()))
    (let ((exp (read-expression port)))
      (if (eof-object? exp)
        (reverse acc)
        (lp (cons exp acc))))))

(define (set-src! obj src)
  "Set the source-properties in src on obj if it supports them."
  (if (supports-source-properties? obj)
    (set-source-properties! obj src))
  obj)

(define *eof* (call-with-input-string "" read))

(define* (escm->scm x)
  (define aux (match-lambda
                ((= eof-object? #t) (list *eof*))
                (('comment src . content) '())
                (('page src . content) '())
                (('sexp src . content)
                 (list
                  (set-src!
                   (match (reverse (apply append (map aux content)))
                     ((rest-sym '#{.}# . rest)
                      (append (reverse rest) rest-sym))
                     (lst (reverse lst)))
                   src)))
                (((and type
                       (or 'quote 'quasiquote 'unquote 'unquote-splicing))
                  src . content)
                 (aux (make-escm src 'sexp (list (make-escm src 'sym type)
                                                 content))))
                ((_ src . content)
                 (list (set-src! content src)))))
  (if (eof-object? x)
    x
    (let ((result (aux x)))
      (unless (null? result) (car result)))))
