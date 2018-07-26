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

(define-module (tests compile)
  #:use-module (tests test-utils)
  #:use-module (language-server scm-utils)
  #:use-module (language-server protocol)
  #:use-module (language-server guile compile)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:export (test))

;; emacs: (put 'run-state 'scheme-indent-function 2)
(define-syntax run-state
  (syntax-rules (<-)
    ((_ state instance)
     instance)
    ((_ state instance <- proc rest ...)
     (run-state state
         (let ((state instance))
           proc) ; replace current instance of state with result of proc
       rest ...))
    ((_ state instance proc rest ...)
     (run-state state
         (let ((state instance))
           proc ; discard value of proc
           state)
       rest ...))))

;; emacs: (put 'run-new-state 'scheme-indent-function 1)
(define-syntax-rule (run-new-state state body ...)
  (run-state state vlist-null body ...))

(define (name->uri name)
  (string-append "file://" name))

;; emacs: (put 'compile 'scheme-indent-function 2)
(define (compile documents name . content)
  (define uri (name->uri name))
  (define old-document
    (or (vhash-ref documents uri)
        (make-document uri name "" #f #f vlist-null #f '())))
  (define text (string-join content "\n"))
  (define document (set-document-text old-document text))
  (compileDocument documents document))

(define (state-document state name)
  (vhash-ref state (name->uri name)))
(define (state-diagnostics state name)
  (document-diagnostics (state-document state name)))
(define (assert-has-diagnostic state name has-severity has-code has-line has-text)
  (define diags (state-diagnostics state name))
  (unless (any (match-lambda
                 (($ <diagnostic>
                     ($ <range>
                        ($ <position> start-line start-char)
                        ($ <position> end-line end-char))
                     severity code source message relatedInfo)
                  (and (equal? has-severity severity)
                       (equal? has-code code)
                       (equal? has-line start-line)
                       (string-contains message has-text))))
               diags)
    (fail "expected to find diagnostic"
          (list has-severity has-code has-line has-text)
          diags)))


(define (test)
  (define m1 "testing/module1.scm")
  (define m2 "testing/module2.scm")

  ;;;
  ;;; Errors/Warnings
  ;;;
  (run-new-state state
    ;; No errors
    <- (compile state m1
         "(+ 1\n2)")
    (assert-equal '() (state-diagnostics state m1))
    ;; Invalid syntax (missing closing paren)
    <- (compile state m1
         "(+ 1\n2")
    (assert-has-diagnostic state m1 DiagnosticSeverityError
                           "scm_i_lreadparen" 1 "end of file")
    ;; Invalid syntax (missing closing quotation mark)
    <- (compile state m1
         "(+ 1 \")")
    (assert-has-diagnostic state m1 DiagnosticSeverityError
                           "scm_lreadr" 0 "end of file")
    ;; Invalid syntax (hash reader error)
    <- (compile state m1
         "(+ 1 #\\invalid)")
    (assert-has-diagnostic state m1 DiagnosticSeverityError
                           ;; FIXME: should 'invalid' instead of '~a'
                           "scm_lreadr" 0 "unknown character name ~a")
    ;; Invalid syntax (unknown hash reader)
    <- (compile state m1
         "(+ 1 #$)")
    (assert-has-diagnostic state m1 DiagnosticSeverityError
                           ;; FIXME: should be '$' instead of '~S'
                           "scm_lreadr" 0 "Unknown # object: ~S")
    ;; Invalid syntax (invalid let)
    <- (compile state m1
         "(let ((a 1)))")
    (assert-has-diagnostic state m1 DiagnosticSeverityError
                           "syntax-error" 0 "bad let")
    ;; Invalid syntax (custom syntax transformer)
    <- (compile state m1
         "
          (define-syntax-rule (test a) (syntax-error \"custom\"))
          (test a)")
    ;; FIXME: should be line 2, not sure if guile is the limiting factor here
    (assert-has-diagnostic state m1 DiagnosticSeverityError
                           "syntax-error" 0 "custom")
    ;; Unbound variable
    <- (compile state m1
         "(+ 1 a)")
    (assert-has-diagnostic state m1 DiagnosticSeverityWarning
                           0 0 "unbound variable `a'"))


  ;;;
  ;;; Modules
  ;;;
  (run-new-state state
    ;; Compile trivial module
    <- (compile state m1
         "(define-module (testing module1) #:export (test1))")
    (assert-equal '() (state-diagnostics state m1))
    ;; Use previously defined module
    <- (compile state m2
         "(define-module (testing module2) #:use-module (testing module1))
             (test1)")
    (assert-equal '() (state-diagnostics state m2))
    ;; Use non-existent export
    <- (compile state m2
         "(define-module (testing module2) #:use-module (testing module1))
             (test1)
             (test2)")
    (assert-has-diagnostic state m2 DiagnosticSeverityWarning
                           0 2 "unbound variable `test2'")
    ;; Add new export
    <- (compile state m1
         "(define-module (testing module1) #:export (test1 test2))")
    (assert-equal '() (state-diagnostics state m2))
    ;; Remove old export
    <- (compile state m1
         "(define-module (testing module1) #:export (test2))")
    (assert-has-diagnostic state m2 DiagnosticSeverityWarning
                           0 1 "unbound variable `test1'")
    ;; Import more modules
    <- (compile state m2
         "(define-module (testing module2) 
                         #:use-module (testing module1)
                         #:use-module (srfi srfi-1))
             (any #f #f)
             (test2)")
    (assert-equal '() (state-diagnostics state m2))
    <- (compile state m2
         "(define-module (testing module2) 
                         #:use-module (testing module1)
                         #:use-module (srfi srfi-1)
                         #:use-module (ice-9 textual-ports))
             (any get-string-all #f)
             (test2)")
    (assert-equal '() (state-diagnostics state m2)))
  ;; Non-existent imports
  (run-new-state state
    (newline)
    (newline)
    (newline)
    ;; First a working base case
    <- (compile state m1
         "(define-module (testing module1) 
                         #:use-module (srfi srfi-1))
          (every #f '())")
    (display state) (newline)
    (assert-equal '() (state-diagnostics state m1))
    ;; Then import a non-existent module
    <- (compile state m1
         "(define-module (testing module1) 
                         #:use-module (srfi srfi-1)
                         ;; Intentional typo: Missing `s' of `ports'
                         #:use-module (ice-9 textual-port))
          (every get-string-all '())")
    (display state) (newline)
    (assert-has-diagnostic state m1 DiagnosticSeverityError
                           "misc-error" 0 "no code for module (ice-9 textual-port)")
    ;; And then with the fixed module name
    <- (compile state m1
         "(define-module (testing module1) 
                         #:use-module (srfi srfi-1)
                         #:use-module (ice-9 textual-ports))
          (every get-string-all '())")
    (display state) (newline)
    (assert-equal '() (state-diagnostics state m1)))
  ;; Filtered imports
  (run-new-state state
    ;; Compile module without filter
    <- (compile state m1
         "(define-module (testing module1) 
                         #:use-module (srfi srfi-1))
          (any #f '())
          (every #f '())")
    (assert-equal '() (state-diagnostics state m1))
    <- (compile state m1
         "(define-module (testing module1) 
                         #:use-module ((srfi srfi-1) #:select (every)))
          (any #f '())
          (every #f '())")
    (assert-has-diagnostic state m1 DiagnosticSeverityWarning
                           0 2 "unbound variable `any'")
    (assert-equal (list (car (state-diagnostics state m1))) ; one element list
                  (state-diagnostics state m1))))
