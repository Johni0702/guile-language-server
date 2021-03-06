#!/bin/env guile2.2
# vim: ft=scheme.guile
!#

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

(define in (current-input-port))
(define out (current-output-port))

(current-output-port (open-file "ls.stdout.log" "w0"))

(add-to-load-path (dirname (current-filename)))
(use-modules (language-server guile server))

(main in out)

(newline)
(newline)
(display "bye")
(newline)
