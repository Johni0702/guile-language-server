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

(define-module (tests test-utils)
  #:use-module (srfi srfi-1)
  #:export (assert-equal
            fail))

(define (fail . args)
  (apply throw (cons 'assertion-error args)))

(define (assert-equal expected actual)
  (unless (equal? expected actual)
    (begin
      (when (and (string? expected) (string? actual))
        (display "Expected to equal:") (newline)
        (for-each (let ((line 0))
                    (lambda (e+a)
                      (define expected (car e+a))
                      (define actual (cadr e+a))
                      (unless (equal? expected actual)
                        (display "Line: ") (display (+ 1 line)) (newline)
                        (display "E: ") (write expected) (newline)
                        (display "A: ") (write actual) (newline))
                      (set! line (+ 1 line))))
                  (zip (string-split expected #\newline)
                       (string-split actual #\newline))))
      (fail "expected to equal" expected actual))))
