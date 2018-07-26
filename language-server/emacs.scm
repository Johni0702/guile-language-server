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

(define-module (language-server emacs)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)

  #:export (read-emacs-dir-locals
            read-emacs-dir-locals-from
            find-emacs-magic-comments))

(define* (read-emacs-dir-locals dir type #:key (mode 'scheme-mode))
  (let* ((file-name (string-append dir "/.dir-locals.el"))
         (parent (string-append dir "/.."))
         (parent_ino (stat:ino (stat parent)))
         (current_ino (stat:ino (stat dir))))
    (append
     (if (file-exists? file-name)
       (call-with-input-file file-name
         (lambda (port) (read-emacs-dir-locals-from port type #:mode mode)))
       '())
     (if (= current_ino parent_ino)
       '()
       (read-emacs-dir-locals parent type #:mode mode)))))

(define* (read-emacs-dir-locals-from port type #:key (mode 'scheme-mode))
  (define filter-for-type
    (match-lambda
      ((((= (lambda (it) (eq? it type)) #t) . rule) . rest)
       (cons rule (filter-for-type rest)))
      ((_ . rest)
       (filter-for-type rest))
      (_
       '())))
  (catch #t
    (lambda ()
      (let* ((root (read port))
             (specific-mode (assq-ref root mode))
             (nil-mode (assq-ref root 'nil)))
        (append (filter-for-type specific-mode)
                (filter-for-type nil-mode))))
    (lambda (key . args)
      (display "Failed to read emacs-dir-locals from ")
      (display (port-filename port)) (display ": ")
      (write key) (display " ") (write args) (newline)
      '())))

(define (find-emacs-magic-comments lst)
  (fold (lambda (exp acc)
          (match exp
            (('comment _ . str*)
             (define str (string-trim (string-trim str* #\;) #\space))
             (define str-exp
               (or (and (string-prefix? "eval:" str) (string-drop str 5))
                   (and (string-prefix? "emacs:" str) (string-drop str 6))))
             (define exp
               (catch #t
                 (lambda () (call-with-input-string str-exp read))
                 (lambda err #f)))
             (if exp
               (cons exp acc)
               acc))
            (('sexp _ . exps)
             (append (find-emacs-magic-comments exps) acc))
            (_ acc)))
        '()
        (cons* 'sexp #f lst)))
