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

(define-module (tests extended-scheme)
  #:use-module (tests test-utils)
  #:use-module (language-server guile extended-scheme)
  #:use-module (system base compile)
  #:use-module (system base language)
  #:use-module (ice-9 textual-ports)
  #:export (test))

(define-syntax-rule (expect-error body ...)
  (catch #t
    (lambda ()
      body ...
      (fail "expected an exception to be thrown"))
    (lambda args args)))

(define (string->scm str)
  (call-with-input-string str (lambda (port) (read port))))

(define (to-string obj)
  (call-with-output-string (lambda (port) (write obj port))))

(define (test-read-error-matches . args)
  (for-each
   (lambda (str)
     (assert-equal (expect-error (string->scm str))
                   (expect-error (escm->scm (string->escm str)))))
   args))

(define (make-default-env)
  (define module (default-environment (current-language)))
  (define name '(test extended-scheme test-case))
  ;; Make sure the name is fixed because it is used in module-gensym
  (set-module-name! module name)
  ;; Also register the module because psyntax is using resolve-module
  (nested-define-module! (resolve-module '() #f) name module)
  module)

(define (compile-to-tree-il x)
  (compile x #:to 'tree-il #:env (make-default-env)))

(define (test-read-result-matches . args)
  (for-each
   (lambda (str)
     (define scm (string->scm str))
     (define escm (string->escm str))
     (assert-equal scm (escm->scm escm)))
   args))

(define (test)
  (test-read-result-matches
   ""
   "  "
   "a"
   " a"
   "'a"
   "'(a)"
   "`a"
   "`(a ,b ,@(c d))"
   "(a)"
   "(a b)"
   "(a b c)"
   "(a (b c))"
   " (  a ( b c ) ) "
   "(define (a b . c) c)"
   "(a 'b)"
   "\"b\""
   "(a \"b\")"
   "#t" "#f" "#nil"
   "#\\newline"
   "#\\("
   "#\\)"
   "(a#|comment\n|#b)"
   "(;comment\na)"
   "'"
   "(begin (define-syntax-rule (a this) 2))"
   (string-append "(begin\n"
                  (call-with-input-file (current-filename) get-string-all)
                  "\n)"))


  (test-read-error-matches
   "`"
   ","
   "' a"
   "("
   ")"
   "\"a"
   "#|"))
