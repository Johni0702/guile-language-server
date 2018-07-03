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

(define-module (language-server guile compile)
  #:use-module (language-server protocol)
  #:use-module (language-server scm-utils)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match)
  #:use-module (system base compile)
  #:use-module (system base language)

  #:export (<document> 
             make-document
             document?
                       uri document-uri
                       name document-name
                       text document-text set-document-text
                       tree-il document-tree-il set-document-tree-il
                       bytecode document-bytecode set-document-bytecode
                       modules document-modules set-document-modules
                       env document-env set-document-env
                       diagnostics document-diagnostics set-document-diagnostics

                       make-empty-document

                       compile-single-document
                       compileDocument))

(define-immutable-record-type <document>
  (make-document uri name text tree-il bytecode modules env diagnostics)
  document?
  (uri document-uri)
  (name document-name)
  (text document-text set-document-text)
  (tree-il document-tree-il set-document-tree-il)
  (bytecode document-bytecode set-document-bytecode)
  (modules document-modules set-document-modules)
  (env document-env set-document-env)
  (diagnostics document-diagnostics set-document-diagnostics))

(define (make-empty-document uri)
  (define name (uri->name uri (map path->uri %load-path)))
  (make-document uri name "" #f #f vlist-null #f '()))

(define (replace-module! name module)
  (define child-name (last name))
  (define parent-name (drop-right name 1))
  (define parent (resolve-module parent-name #f))
  (define submodules (module-submodules parent))
  (define prev-module (hashq-ref submodules child-name))
  (if module
    (begin
      (hashq-set! submodules child-name module)
      (nested-define! parent `(,child-name) module))
    (begin
      (hashq-remove! submodules child-name)
      (nested-remove! parent `(,child-name))))
  prev-module)

(define-syntax-rule
    (swap-modules! prev-ones new-ones)
  (set! prev-ones
        (map (match-lambda
               ((name . module)
                (display "Replacing module ") (display name)
                (display " with ") (display module) (newline)
                (cons name (replace-module! name module))))
             new-ones)))

(define (%with-modules modules thunk)
  (define new-modules (map (lambda (m) (cons (module-name m) m)) modules))
  (define prev-modules '())
  (dynamic-wind
      (lambda () (swap-modules! prev-modules new-modules))
      thunk
      (lambda () (swap-modules! new-modules prev-modules))))

(define-syntax-rule
    (with-modules modules body ...)
  (%with-modules modules (lambda () body ...)))

(define (%track-defined-modules overwritable-modules into thunk)
  (let* ((replaced-modules '())
         (define-module-original (@@ (guile) define-module*))
         (define-module-wrapper
             (lambda (name . rest)
               (if (or (not (null? autoloads-in-progress))
                       (member name overwritable-modules))
                 (begin
                   (apply define-module-original (cons name rest)))
                 (let* ((replaced-module (replace-module! name #f))
                        (module (apply define-module-original (cons name rest))))
                   (set! replaced-modules
                         (acons name replaced-module replaced-modules))
                   (into module)
                   module))))
         (result (dynamic-wind
                     (lambda ()
                       (set! (@@ (guile) define-module*) define-module-wrapper))
                     thunk
                     (lambda ()
                       (set! (@@ (guile) define-module*) define-module-original)
                       (for-each (lambda (name+module)
                                   (replace-module! (car name+module)
                                                    (cdr name+module)))
                                 replaced-modules)))))
    result))

(define-syntax-rule
    (track-defined-modules overwritable-modules into body ...)
  (%track-defined-modules overwritable-modules into (lambda () body ...)))

(define nullRange
  (make-range (make-position 0 0)
              (make-position 0 1)))

(define (warning-string->diagnostic str)
  (define parts (string-split str #\:))
  (match parts
    (("WARNING" _ . rest)
     (make-diagnostic
      nullRange
      DiagnosticSeverityWarning
      0
      "guile"
      (string-drop (string-join rest ":") 1)
      '()))
    ((_ str-line str-char " warning" . rest)
     (define line (- (string->number str-line) 1))
     (define char (string->number str-char))
     (make-diagnostic
      (make-range (make-position line char)
                  (make-position line (+ char 1)))
      DiagnosticSeverityWarning
      0
      "guile"
      (string-drop (string-join rest ":") 1)
      '()))
    (("") #f)
    (parts
     (display (string-join parts ":"))
     (newline)
     #f)))

(define (%diagnose-warnings thunk)
  (begin
    (define result #f)
    (define warnings-string
      (call-with-output-string
       (lambda (port)
         (set! result (parameterize ((current-warning-port port)) (thunk))))))
    (cons
     (filter identity
             (map warning-string->diagnostic
                  (string-split warnings-string #\nl)))
     result)))

(define-syntax-rule
    (diagnose-warnings body ...)
  (%diagnose-warnings (lambda () body ...)))

(define (%diagnose-errors thunk)
  (define-syntax handle
    (syntax-rules
        ()
      ((handle (key args handler ...) thunk)
       (catch key (lambda () thunk) (lambda args handler ...)))
      ((handle (key args handler ...) handlers ...)
       (catch key (lambda () (handle handlers ...)) (lambda args handler ...)))))
  (define-syntax-rule
      (ret-error args ...)
    (cons (list (make-diagnostic args ...)) #f))
  (handle
   (#t (key . args)
       (ret-error
        nullRange
        DiagnosticSeverityError
        (symbol->string key)
        "guile"
        (call-with-output-string
         (lambda (port) (print-exception port #f key args)))
        '()))
   ('read-error (_ where text . extra)
                (match
                    (string-split text #\:)
                  ((_ str-line str-char . rest)
                   (define line (- (string->number str-line) 1))
                   (define char (string->number str-char))
                   (ret-error
                    (make-range (make-position line (- char 1))
                                (make-position line char))
                    DiagnosticSeverityError
                    where
                    "guile"
                    (string-drop (string-join rest ":") 1)
                    '()))
                  (_ (apply throw (cons* 'read-error where text extra)))))
   ('syntax-error (_ who what where form subform . extra)
                  (define range
                    (if (eq? where #f)
                      nullRange
                      (let* ((start-position (source-properties->position where))
                             ;; TODO somehow use `form` to generate range
                             (end-position (make-position (position-line start-position)
                                                          (position-char start-position))))
                        (make-range start-position end-position))))
                  (display (cons* who what where form subform extra))
                  (ret-error
                   range
                   DiagnosticSeverityError
                   "syntax-error"
                   "guile"
                   (string-append what ": " (call-with-output-string
                                             (lambda (port) (write form port))))
                   '()))
   (cons '() (thunk))))

(define-syntax-rule
    (diagnose-errors body ...)
  (%diagnose-errors (lambda () body ...)))

(define (diagnose-compile src existing-modules . args)
  (define modules '())
  (match
      (diagnose-warnings
       (diagnose-errors
        (with-modules
         (vhash-values existing-modules)
         (track-defined-modules
          (vhash-keys existing-modules)
          (lambda (module) (set! modules (cons module modules)))
          (if (port? src)
            (apply read-and-compile (cons src args))
            (apply compile (cons src args)))))))
    ((warnings errors . result)
     (list (append errors warnings)
           (alist->vhash (map (lambda (m) (cons (module-name m) m)) modules))
           result))))

(define (compile-single-document all-modules all-documents document)
  (define env (default-environment (current-language)))
  (display "Compiling ") (display (document-uri document)) (newline)
  (match
      (call-with-input-string
       (document-text document)
       (lambda (port)
         (set-port-filename! port (document-name document))
         (diagnose-compile
          port
          (vhash-remove-all (document-modules document) all-modules)
          #:to 'tree-il
          #:env env
          #:opts %auto-compilation-options)))
    ((diagnostics-scm modules tree-il)
     (define cenv
       ;; FIXME: Looks like atm there's no way to get the continuation
       ;;        environment from any of the exported compile procedures.
       ;;        So for now, we cheat and assume the environment to be the
       ;;        first registered module.
       (match (vhash->alist modules)
         (() env)
         (lst (cdr (last lst)))))
     (match
         (if tree-il
           (diagnose-compile
            tree-il
            (vhash-put-all modules all-modules)
            #:from 'tree-il
            #:to 'bytecode
            #:env cenv
            #:opts %auto-compilation-options)
           '(() #f #f))
       ((diagnostics-tree-il _ bytecode)
        (define diagnostics (append diagnostics-scm diagnostics-tree-il))
        (display "Diagnostics: ") (display diagnostics) (newline)
        (display "Modules: ") (display (vhash-values modules)) (newline)
        (display "Uses: ")
        (display (apply append (map (lambda (module)
                                      (map module-name (module-uses module)))
                                    (vhash-values modules))))
        (newline)
        ;; Finally compile to value (i.e. run) to allow xref to function.
        (diagnose-compile
         bytecode
         (vhash-put-all modules all-modules)
         #:from 'bytecode
         #:to 'value
         #:env cenv ;; FIXME: use proper cenv (see above)
         #:opts %auto-compilation-options)
        (cons modules
              (set-fields
               document
               ((document-modules) modules)
               ((document-diagnostics) diagnostics)
               ((document-env) cenv)
               ((document-bytecode) bytecode)
               ((document-tree-il) tree-il))))))))

(define (build-dependents-tree all-modules all-documents document)
  (define modules-in-document (vhash-keys (document-modules document)))
  (define dependent-modules
    (vhash-filter
     (lambda (name module)
       (define deps (map module-name (module-uses module)))
       (any (lambda (m) (member m deps)) modules-in-document))
     all-modules))
  (cons
   document
   (vhash-values
    (vhash-map
     (lambda (name module)
       (define document (vhash-fold
                         (lambda (uri document acc)
                           (if (vhash-ref (document-modules document) name)
                             document
                             acc))
                         #f
                         all-documents))
       (build-dependents-tree all-modules all-documents document))
     dependent-modules))))

(define (compileDocument all-modules all-documents document)
  (define build-plan
    (unique (flatten-pre (build-dependents-tree
                          all-modules all-documents document))))
  (display "Update for ") (display (document-uri document)) (newline)
  (display "Build plan: ") (display (map document-uri build-plan)) (newline)
  (fold
    (lambda (document acc)
      (match
        acc
        ((all-modules . all-documents)
         (match
           (compile-single-document all-modules all-documents document)
           ((changed-modules . changed-document)
            (define uri (document-uri changed-document))
            (cons
              (vhash-put-all changed-modules all-modules)
              (vhash-put uri changed-document all-documents)))))))
    (cons all-modules all-documents)
    build-plan))
