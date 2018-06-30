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

(define-module (language-server guile xref)
  #:use-module (language-server guile compile)
  #:use-module (language-server protocol)
  #:use-module (language-server scm-utils)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match)
  #:use-module (system base compile)
  #:use-module (system base language)
  #:use-module (system vm program)
  #:use-module (language tree-il)

  #:export (find-definition))

(define (get-word-at document position)
  (define lines (text->lines (document-text document)))
  (define line (list-ref lines (position-line position)))
  (define char (position-char position))
  (define before (string->list (string-take line char)))
  (define after (string->list (string-drop line char)))
  (define delimiters '(#\space #\tab #\( #\)))
  (define not-delim (lambda (c) (not (member c delimiters))))
  (define word-before (reverse (take-while not-delim (reverse before))))
  (define word-after (take-while not-delim after))
  (list->string (append word-before word-after)))

(define (tree-il-fold-with-source down up init tree-il)
  "tree-il-fold with an additional src as the first parameter to down and up."
  (cdr (tree-il-fold
    (lambda (x acc)
      (define x-src (tree-il-src x))
      (define src-stack (cons x-src (car acc)))
      (define best-src (find identity src-stack))
      (cons src-stack (down best-src x (cdr acc))))
    (lambda (x acc)
      (define src-stack (car acc))
      (define best-src (find identity src-stack))
      (cons (cdr src-stack) (down best-src x (cdr acc))))
    (cons '() init)
    tree-il)))

(define (get-ref-at document position)
  (define target-line (position-line position))
  (define target-char (position-char position))
  (define tree-il (document-tree-il document))
  (define closest
    (or (cdr 
          (tree-il-fold
            (lambda (x result) 
              (define src (source-properties->position (tree-il-src x)))
              (define src-line (position-line src))
              (define src-char (position-char src))
              (if (and src-line src-char
                       (<= src-line target-line)
                       (<= src-char target-char)
                       (or (> src-line (cdar result))
                           (and (= src-line (cdar result))
                                (> src-char (caar result))))) 
                (cons (cons src-char src-line) x)
                result))
            (lambda (x result) result)
            (cons (cons -1 -1) #f)
            tree-il))
        ;; In case there is no source info in the tree-il, search all of it
        tree-il))
  (define word (string->symbol (get-word-at document position)))
  (display "Word at position: ") (display word) (newline)
  (display "Searching for exact match in ") (display closest) (newline)
  (tree-il-fold
    (lambda (x result)
      (define name (match 
                     x
                     (($ <toplevel-ref> _ name) name)
                     (($ <lexical-ref> _ name _) name)
                     (_ #f)))
      (if (equal? name word) 
        (begin
          (display "Found reference: ") (display x) (newline)
          x) 
        result))
    (lambda (x result) result)
    #f
    closest))

(define (find-lexical-definition document gensym)
  #f)

(define (find-name-in-load-path name)
  (find
    (lambda (file) (file-exists? file))
    (map
      (lambda (dir) (string-append dir "/" name))
      %load-path)))

(define (name->uri documents name)
  (define document
    (find (lambda (doc) (equal? name (document-name doc)))
          (vhash-values documents)))
  (if document
    (document-uri document)
    (and=> (find-name-in-load-path name)
           (lambda (it) (string-append "file://" it)))))

(define (find-lexical-definition document gensym)
  (display "Looking for gensym ") (display gensym) (display " in tree-il of ")
  (display (document-uri document)) (newline)
  (tree-il-fold-with-source
    (lambda (src x result)
      (match
        x
        (($ <let> _ _ (= (lambda (gensyms) (not (member gensym gensyms))) #f) _ _)
         (display "Found one: ") (display x) (newline)
         src)
        (_ result)))
    (lambda (src x result) result)
    #f
    (document-tree-il document)))

(define (find-tree-il-toplevel-define document name)
  (display "Looking for ") (display name) (display " in tree-il of ")
  (display (document-uri document)) (newline)
  (tree-il-fold-with-source
    (lambda (src x result)
      (match
        x
        (($ <toplevel-define> _ (= (lambda (n) (eq? n name)) #t) _)
         (display "Found one: ") (display x) (newline)
         src)
        (_ result)))
    (lambda (src x result) result)
    #f
    (document-tree-il document)))

(define (module->document documents module)
  (if module
    (let ((name (module-name module)))
      (find
        (lambda (document) (vhash-ref (document-modules document) name))
        documents))
    #f))

(define (find-toplevel-definition documents document name)
  ;; We can't really know the module, so let's just use the first one and hope
  ;; people declare one per file.
  (define modules (vhash->alist (document-modules document)))
  (define module (if (null? modules)
                   (document-env document)
                   (cdar modules)))
  (define proc (module-ref module name #f))
  (if (program? proc)
    ;; For executable code, guile already stores source locations for debugging
    (match
      (program-source proc 0)
      ((_ name start-line . start-char)
       (match
         (last (program-sources proc))
         ((_ _ end-line . end-char)
          (make-location
            (name->uri documents name)
            (make-range
              (make-position start-line start-char)
              (make-position end-line end-char))))))
      ;; No jumping to C code (or whatever else can cause this) for now
      (#f #f))
    ;; For non-procedure toplevels try looking at tree-ils.
    ;; FIXME: If they're from external modules, this might fail because of the
    ;;        renamer. Though it should be possible to build a reverse mapping.
    ;; FIXME: doesn't respect duplicate binding handler
    ;; FIXME: ignores custom binder
    (and=>
      (module->document
        (vhash-values documents)
        (or (if (hashq-ref (module-obarray module) name) module #f)
            (find-mapped
              (lambda (module)
                (if (module-variable module name) module #f))
              (module-uses module))))
      (lambda (document)
        (and=>
          (find-tree-il-toplevel-define document name)
          (lambda (src-pos)
            (define position (source-properties->position src-pos))
            (display "Result: ") (display src-pos) (newline)
            (make-location
              (document-uri document)
              (make-range position position))))))))

(define (find-definition documents document position)
  (define ref (get-ref-at document position))
  (define result (match 
                   ref
                   (($ <toplevel-ref> _ name) 
                    (find-toplevel-definition documents document name))
                   (($ <lexical-ref> _ _ gensym)
                    (and=>
                      (and=>
                        (find-lexical-definition document gensym)
                        source-properties->position)
                      (lambda (position)
                        (display "Result: ") (display position) (newline)
                        (make-location
                          (document-uri document)
                          (make-range position position)))))
                   (#f #f)))
  result)

