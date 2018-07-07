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

(define-module (language-server guile formatter)
  #:use-module (language-server guile extended-scheme)
  #:use-module (language-server emacs)
  #:use-module (system base compile)
  #:use-module (system base language)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:export (escm-list->indented-string))

(define (line escm)
  (assoc-ref (cadr escm) 'line))

(define (newline-count str)
  (string-count str #\newline))

(define (comment? escm) (eq? 'comment (car escm)))
(define (hash? escm) (eq? 'hash (car escm)))
(define (sym? escm) (eq? 'sym (car escm)))
(define (get-sym escm) (cddr escm))
(define (get-hash escm) (cddr escm))

(define (times x n)
  (if (zero? n)
    '()
    (cons x (times x (- n 1)))))

(define (newlines n indent)
  (if (zero? n)
    " "
    (string-append (make-string n #\newline)
                   (make-string indent #\space))))

(define (last-line str)
  (substring str (or (string-rindex str #\newline) 0)))

(define* (escm-list->indented-string lst
                                     #:key
                                     (indent-func* %default-indent)
                                     (newline-on-eof? #t)
                                     (emacs-dir-locals-path #f)
                                     (check-for-emacs-magic? #t))
  (define emacs-evals
    (append (if check-for-emacs-magic?
              (find-emacs-magic-comments lst)
              '())
            (if emacs-dir-locals-path
              (read-emacs-dir-locals emacs-dir-locals-path 'eval)
              '())))
  (define indent-func
    (append (filter identity (map parse-emacs-indent emacs-evals))
            indent-func*))
  (let format ((tree (cons* 'top '() lst)) (indent 0))
    (match tree
      (('top _ . lst)
       (let lp ((lst lst)
                (prev-line 0))
         (if (null? lst)
           (if newline-on-eof? "\n" "")
           (let* ((exp (car lst))
                  (this-line (line exp))
                  (formatted-exp (format exp indent))
                  (this-line-offset (- this-line prev-line)))
             (string-append
              (if (zero? this-line)
                ""
                (newlines this-line-offset indent))
              formatted-exp
              (lp (cdr lst)
                  (+ this-line (newline-count formatted-exp))))))))
      (('sexp src)
       "()")
      (('sexp src proc)
       (string-append "("
                      (format proc (+ 1 indent))
                      ")"))
      (('sexp src proc first . rest)
       (define proc-sym (and (sym? proc) (get-sym proc)))
       (define proc-keyword? (and (hash? proc) (keyword? (get-hash proc))))
       (define deep
         (or (and proc-sym
                  (or (assq-ref indent-func proc-sym)
                      (and (string-prefix? "def" (symbol->string proc-sym)) 1)))
             (and proc-keyword? 1)))
       (define first-line-offset (- (line first) (line proc)))
       (define proc-formatted (format proc (+ 1 indent)))
       (define end-indent (+ indent
                             (string-length (last-line proc-formatted))
                             2))
       (define arg-indent
         (if (and (zero? first-line-offset)
                  (or (not deep) (zero? deep)))
           end-indent
           (+ indent (if (and deep (not proc-keyword?)) 2 1))))
       (string-append
        "("
        proc-formatted
        (let lp ((args (cons first rest))
                 (prev-line (+ (line proc) (newline-count proc-formatted)))
                 (prev-indent end-indent)
                 (deep deep))
          (if (null? args)
            ""
            (let* ((arg (car args))
                   (this-start-line (line arg))
                   (this-line-offset (- this-start-line prev-line))
                   (this-indent (if (zero? this-line-offset)
                                  prev-indent
                                  (+ arg-indent
                                     (if (and deep (> deep 0)) 2 0))))
                   (this-formatted (format arg this-indent))
                   (this-newlines (newline-count this-formatted))
                   (this-end-line (+ (line arg) this-newlines))
                   (this-end-indent (if (zero? this-newlines)
                                      (+ this-indent
                                         (string-length this-formatted)
                                         1)
                                      arg-indent)))
              (string-append (newlines this-line-offset this-indent)
                             this-formatted
                             (lp (cdr args)
                                 this-end-line
                                 this-end-indent
                                 (and deep (- deep (if (comment? arg)
                                                     0
                                                     1))))))))
        (if (comment? (last (cons first rest)))
          (newlines 1 arg-indent)
          "")
        ")"))
      (('sym src . sym)
       (or (assq-ref src 'raw)
           (and (symbol? sym) (symbol->string sym))
           (number->string sym)))
      (('quote src . content)
       (string-append "'" (format content (+ 1 indent))))
      (('quasiquote src . content)
       (string-append "`" (format content (+ 1 indent))))
      (('unquote src . content)
       (string-append "," (format content (+ 1 indent))))
      (('unquote-splicing src . content)
       (string-append ",@" (format content (+ 2 indent))))
      (('hash src . content)
       (assq-ref src 'raw))
      (('page src . _)
       "\f")
      (('string src . str)
       (or (assq-ref src 'raw)
           (call-with-output-string (lambda (port) (write str port)))))
      (('comment src . str)
       (string-append ";" str)))))

(define parse-emacs-indent
  (match-lambda
    (('put ('quote sym) ''scheme-indent-function value)
     (cons sym value))
    (_ #f)))

(define (register-emacs-indent x current-indents)
  (cons (parse-emacs-indent x) current-indents))

(define %emacs-default-indent
  (fold
   register-emacs-indent
   '()
   ;;; These defaults have been adapted from the GNU Emacs scheme.el file.
   ;;; scheme.el --- Scheme (and DSSSL) editing mode    -*- lexical-binding: t; -*-

   ;; Copyright (C) 1986-1988, 1997-1998, 2001-2018 Free Software
   ;; Foundation, Inc.

   ;; Author: Bill Rozas <jinx@martigny.ai.mit.edu>
   ;; Adapted-by: Dave Love <d.love@dl.ac.uk>
   ;; Keywords: languages, lisp

   ;; This file is part of GNU Emacs.

   ;; GNU Emacs is free software: you can redistribute it and/or modify
   ;; it under the terms of the GNU General Public License as published by
   ;; the Free Software Foundation, either version 3 of the License, or
   ;; (at your option) any later version.

   ;; GNU Emacs is distributed in the hope that it will be useful,
   ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
   ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   ;; GNU General Public License for more details.

   ;; You should have received a copy of the GNU General Public License
   ;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
   '((put 'begin 'scheme-indent-function 0)
     (put 'case 'scheme-indent-function 1)
     (put 'delay 'scheme-indent-function 0)
     (put 'do 'scheme-indent-function 2)
     (put 'lambda 'scheme-indent-function 1)
     (put 'let* 'scheme-indent-function 1)
     (put 'letrec 'scheme-indent-function 1)
     (put 'let-values 'scheme-indent-function 1) ; SRFI 11
     (put 'let*-values 'scheme-indent-function 1) ; SRFI 11
     (put 'sequence 'scheme-indent-function 0) ; SICP, not r4rs
     (put 'let-syntax 'scheme-indent-function 1)
     (put 'letrec-syntax 'scheme-indent-function 1)
     (put 'syntax-rules 'scheme-indent-function 1)
     (put 'syntax-case 'scheme-indent-function 2) ; not r5rs
     (put 'library 'scheme-indent-function 1) ; R6RS

     (put 'call-with-input-file 'scheme-indent-function 1)
     (put 'with-input-from-file 'scheme-indent-function 1)
     (put 'with-input-from-port 'scheme-indent-function 1)
     (put 'call-with-output-file 'scheme-indent-function 1)
     (put 'with-output-to-file 'scheme-indent-function 1)
     (put 'with-output-to-port 'scheme-indent-function 1)
     (put 'call-with-values 'scheme-indent-function 1) ; r5rs?
     (put 'dynamic-wind 'scheme-indent-function 3) ; r5rs?

     ;; R7RS
     (put 'when 'scheme-indent-function 1)
     (put 'unless 'scheme-indent-function 1)
     (put 'letrec* 'scheme-indent-function 1)
     (put 'parameterize 'scheme-indent-function 1)
     (put 'define-values 'scheme-indent-function 1)
     (put 'define-record-type 'scheme-indent-function 1) ;; is 1 correct?
     (put 'define-library 'scheme-indent-function 1)

     ;; SRFI-8
     (put 'receive 'scheme-indent-function 2))))

(define scheme-let-indent 1) ;; FIXME: needs to dynamically be 2 for loop form

(define %guile-default-indent
  (fold
   (lambda (entry acc) (acons (car entry) (cadr entry) acc))
   '()
   ;;; More indentions, this time from GNU Guile's emacs/guile-scheme.el
   ;;; guile-scheme.el --- Guile Scheme editing mode

   ;; Copyright (C) 2001, 2006 Free Software Foundation, Inc.

   ;;;; This library is free software; you can redistribute it and/or
   ;;;; modify it under the terms of the GNU Lesser General Public
   ;;;; License as published by the Free Software Foundation; either
   ;;;; version 3 of the License, or (at your option) any later version.
   ;;;; 
   ;;;; This library is distributed in the hope that it will be useful,
   ;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
   ;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   ;;;; Lesser General Public License for more details.
   ;;;; 
   ;;;; You should have received a copy of the GNU Lesser General Public
   ;;;; License along with this library; if not, write to the Free
   ;;;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   ;;;; 02111-1307 USA

   `((begin 0) (if 1) (cond 0) (case 1) (do 2)
               (match 1) (match-lambda 0) (match-lambda* 0)
               (let ,scheme-let-indent) (let* 1) (letrec 1) (and-let* 1)
               (let-syntax 1) (letrec-syntax 1) (syntax-rules 1) (syntax-case 2)
               (catch 1) (lazy-catch 1) (stack-catch 1)
               (dynamic-wind 3))))

(define %default-indent
  (append %guile-default-indent
          %emacs-default-indent))
