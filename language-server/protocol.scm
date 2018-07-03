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

(define-module (language-server protocol)
  #:use-module (json-rpc)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (RequestCancelled

            <position>
            make-position
            position?
            position-line
            position-char

            <range>
            make-range
            range?
            range-start
            range-end

            <text-edit>
            make-text-edit
            text-edit?
            text-edit-range
            text-edit-text

            <location>
            make-location
            location?
            location-uri
            location-range

            <diagnostic>
            make-diagnostic
            diagnostic?
            diagnostic-range
            diagnostic-severity
            diagnostic-code
            diagnostic-source
            diagnostic-message
            diagnostic-relatedInfo

            DiagnosticSeverityError
            DiagnosticSeverityWarning
            DiagnosticSeverityInformation
            DiagnosticSeverityHint

            <textDocument>
            make-textDocument
            textDocument?
            textDocument-uri
            textDocument-languageId
            textDocument-version
            textDocument-text

            position->scm
            range->scm
            text-edit->scm
            location->scm
            diagnostic->scm

            scm->position
            scm->textDocument

            source-properties->position

            sendRegisterCapability
            sendDiagnostics))

(define RequestCancelled -32800)

(define-record-type <position>
  (make-position line char)
  position?
  (line position-line)
  (char position-char))

(define-record-type <range>
  (make-range start end)
  range?
  (start range-start)
  (end range-end))

(define-record-type <text-edit>
  (make-text-edit range text)
  text-edit?
  (range text-edit-range)
  (text text-edit-text))

(define-record-type <location>
  (make-location uri range)
  location?
  (uri location-uri)
  (range location-range))

(define-record-type <diagnostic>
  (make-diagnostic range severity code source message relatedInfo)
  diagnostic?
  (range diagnostic-range)
  (severity diagnostic-severity)
  (code diagnostic-code)
  (source diagnostic-source)
  (message diagnostic-message)
  (relatedInfo diagnostic-relatedInfo))

(define DiagnosticSeverityError 1)
(define DiagnosticSeverityWarning 2)
(define DiagnosticSeverityInformation 3)
(define DiagnosticSeverityHint 4)

(define-record-type <textDocument>
  (make-textDocument uri languageId version text)
  textDocument?
  (uri textDocument-uri)
  (languageId textDocument-languageId)
  (version textDocument-version)
  (text textDocument-text))


(define position->scm
  (match-lambda (($ <position> line char)
                 `((line . ,line)
                   (character . ,char)))))

(define range->scm
  (match-lambda (($ <range> start end)
                 `((start . ,(position->scm start))
                   (end . ,(position->scm end))))))

(define text-edit->scm
  (match-lambda (($ <text-edit> range text)
                 `((range . ,(range->scm range))
                   (newText . ,text)))))

(define location->scm
  (match-lambda (($ <location> uri range)
                 `((uri . ,uri)
                   (range . ,(range->scm range))))))

(define diagnostic->scm
  (match-lambda (($ <diagnostic> range severity code source message relatedInfo)
                 `((range . ,(range->scm range))
                   (severity . ,severity)
                   (code . ,code)
                   (source . ,source)
                   (message . ,message)
                   ;; TODO relatedInfo
                   ))))


(define (scm->position obj)
  (make-position
   (hash-ref obj "line")
   (hash-ref obj "character")))

(define (scm->textDocument obj)
  (make-textDocument
   (hash-ref obj "uri")
   (hash-ref obj "languageId")
   (hash-ref obj "version")
   (hash-ref obj "text")))


(define (source-properties->position where)
  (make-position (assoc-ref where 'line) (assoc-ref where 'column)))


(define (sendRegisterCapability port . registrations)
  (sendMessage port `(;; FIXME: handle response
                      (id . #nil)
                      (method . "client/registerCapability")
                      (params . ((registrations . ,registrations))))))

(define (sendDiagnostics port uri diagnostics)
  (sendNotification port "textDocument/publishDiagnostics"
                    `((uri . ,uri)
                      (diagnostics . ,(map diagnostic->scm diagnostics)))))
