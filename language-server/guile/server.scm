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

(define-module (language-server guile server)
  #:use-module (language-server guile xref)
  #:use-module (language-server protocol)
  #:use-module (language-server scm-utils)
  #:use-module (language-server guile compile)
  #:use-module (language-server guile extended-scheme)
  #:use-module (language-server guile formatter)
  #:use-module (json-rpc)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:export (main))

(define-immutable-record-type <state>
  (make-state rootUri documents shutdown?)
  state?
  (rootUri state-rootUri set-state-rootUri)
  (documents state-documents set-state-documents)
  (shutdown? state-shutdown? set-state-shutdown?))

(define (updateDocument out state uri text)
  (define old-documents (state-documents state))
  (define old-document (or (vhash-ref old-documents uri)
                           (make-empty-document uri)))
  (define new-document (set-document-text old-document text))
  (define new-documents (compileDocument old-documents new-document))
  (vhash-for-each
   (lambda (uri document)
     ;; FIXME: only send for changed documents
     (display "Sending diagnostics for ")
     (display (document-uri document)) (newline)
     (display (document-diagnostics document)) (newline)
     (sendDiagnostics out uri (document-diagnostics document)))
   new-documents)
  (set-state-documents state new-documents))

(define (handleInitialize in out id params)
  (define rootUri (hash-ref params "rootUri")) ;; FIXME: handle rootPath
  ;; TODO handle client capabilities
  (define serverCapabilities
    '((textDocumentSync . ((openClose . #t)
                           (change . 1)))))
  ;; FIXME: always assumes uris of file:// kind
  (add-to-load-path (string-drop rootUri 7))
  (sendResult out id `((capabilities . ,serverCapabilities)))
  (make-state rootUri vlist-null #f))

(define (handleInitialized out)
  (define guile-document-selector `(((language . "guile"))))
  (sendRegisterCapability
   out
   `((id . "1")
     (method . "textDocument/didChange")
     (registerOptions . ((textDocumentSelector . ,guile-document-selector)
                         (syncKind . 1))))))

(define (waitForInit in out)
  (match (readMessage in)
    (($ <request> id "initialize" params)
     (handleInitialize in out id params))
    (($ <request> _ "exit" _) #f)
    (($ <request> #nil _ _)
     (waitForInit in out))
    (($ <request> id _ _)
     (sendError out id ServerNotInitialized "Expected 'initialize' message.")
     (waitForInit in out))))

(define (mainLoop in out state)
  ;; TODO handle batches
  (define result
    (match (readMessage in)
      (($ <response> id result error)
       ;; TODO handle responses
       state)
      (($ <request> _ "initialized" _)
       (handleInitialized out)
       state)
      (($ <request> _ "exit" _) #f)
      (($ <request> id "shutdown" _)
       (sendResult out id #nil)
       (set-state-shutdown? state #t))
      (($ <request> #f "textDocument/didOpen" params)
       (define textDocument (scm->textDocument (hash-ref params "textDocument")))
       (define uri (textDocument-uri textDocument))
       (define text (textDocument-text textDocument))
       (display "textDocument/didOpen: ") (display uri) (newline)
       (updateDocument out state uri text))
      (($ <request> #f "textDocument/didChange" params)
       (define versioned-text-document-id (hash-ref params "textDocument"))
       (define version (hash-ref versioned-text-document-id "version"))
       (define uri (hash-ref versioned-text-document-id "uri"))
       ;; TODO implement incremental updates
       (define text (hash-ref (car (hash-ref params "contentChanges")) "text"))
       (display "textDocument/didChange: ") (display uri) (newline)
       (updateDocument out state uri text))
      (($ <request> id "textDocument/definition" params)
       (define uri (hash-ref (hash-ref params "textDocument") "uri"))
       (define position (scm->position (hash-ref params "position")))
       (define documents (state-documents state))
       (define document (vhash-ref documents uri))
       ;; FIXME: handle missing document
       (display "textDocument/definition ") (display uri)
       (display " at ") (display position) (newline)
       (let ((location (find-definition documents document position)))
         (sendResult out id (if location (location->scm location) #nil)))
       state)
      (($ <request> id "textDocument/formatting" params)
       (define uri (hash-ref (hash-ref params "textDocument") "uri"))
       (define documents (state-documents state))
       (define document (vhash-ref documents uri))
       ;; FIXME: handle missing document
       (display "textDocument/formatting ") (display uri) (newline)
       (let* ((old-text (document-text document))
              (full-range (make-range
                           (make-position 0 0)
                           (make-position (+ 1 (string-count old-text
                                                             #\newline))
                                          0)))
              (escm (string->escm-list old-text))
              (new-text (escm-list->indented-string
                         escm
                         ;; FIXME: always assumes uris of file:// kind
                         #:emacs-dir-locals-path (dirname
                                                  (string-drop uri 7))))
              (text-edit (make-text-edit full-range new-text)))
         (sendResult out id (list (text-edit->scm text-edit))))
       state)
      (($ <request> #f method _)
       (display (string-append "Got notification with unknown method: " method "\n"))
       state)
      (($ <request> id method _)
       (display (string-append "Got request for unknown method: " method "\n"))
       (sendError out id MethodNotFound "Method not found")
       state)))
  (if (eq? result #f)
    #f
    (mainLoop in out result)))

(define (main in out)
  (define initialState (waitForInit in out))
  (if (eq? initialState #f)
    #f
    (mainLoop in out initialState)))
