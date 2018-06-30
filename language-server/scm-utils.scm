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

(define-module (language-server scm-utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 vlist)
  #:export (text->lines
            find-mapped

            vhash-ref
            vhash-put
            vhash-put-all
            vhash-remove-all
            vhash-filter
            vhash-map
            vhash-keys
            vhash-values
            vhash-for-each
            vhash->alist

            uri->name
            path->uri

            flatten-pre
            unique))

(define (text->lines text)
  (string-split text #\nl)) ;; FIXME: should also handle CR and CRNL

(define (find-mapped proc lst)
  (fold (lambda (e acc) (if acc acc (proc e))) #f lst))

(define (vhash-ref vhash key) (and=> (vhash-assoc key vhash) cdr))

(define (vhash-put-all from vhash) 
  (vhash-fold
    (lambda (key value result) (vhash-put key value result))
    vhash
    from))

(define (vhash-remove-all to-be-removed vhash) 
  (vhash-filter
    (lambda (key _) (not (vhash-ref to-be-removed key)))
    vhash))

(define (vhash-put key value vhash) 
  (vhash-cons key value (vhash-delete key vhash)))

(define (vhash-filter proc vhash)
  (vhash-fold (lambda (key value acc)
                (if (proc key value) (vhash-cons key value acc) acc))
              vlist-null
              vhash))

(define (vhash-map proc vhash)
  (vhash-fold (lambda (key value acc)
                (vhash-cons key (proc key value) acc))
              vlist-null
              vhash))

(define (vhash-for-each proc vhash)
  (vhash-fold (lambda (key value _) (proc key value)) #f vhash))

(define (vhash-keys vhash)
  (vhash-fold (lambda (key _ acc) (cons key acc)) '() vhash))

(define (vhash-values vhash)
  (vhash-fold (lambda (_ value acc) (cons value acc)) '() vhash))

(define (vhash->alist vhash)
  (vhash-fold (lambda (key value acc) (cons (cons key value) acc)) '() vhash))

(define (uri->name uri path)
  ;; Find longest matching prefix of uri in path and use remaining part of uri
  (string-drop
    uri
    (+ 1 (string-length
           (fold
             (lambda (prefix best-prefix)
               (if (and (> (string-length prefix) (string-length best-prefix))
                        (string-prefix? prefix uri))
                 prefix
                 best-prefix))
             "file://"
             path)))))

(define (path->uri path)
  (string-append
    "file://"
    (if (absolute-file-name? path)
      path
      ;; FIXME: breaks for any non-trivial relative path i.e. ones with dot(s)
      ;;        also assumes (getcwd) to be absolute (which might not be true?)
      (let ((cwd (getcwd)))
        (if (equal? path ".") ;; special case "." so at least that works
          cwd
          (string-append cwd "/" path))))))

(define (flatten-pre tree)
  (define (aux node acc)
    (cons (car node) (fold aux acc (cdr node))))
  (aux tree '()))

(define (unique lst)
  ;; Note: not using fold-right to preserve order of first occurance
  (reverse (fold (lambda (e acc)
                   (if (member e acc) acc (cons e acc)))
                 '() lst)))
