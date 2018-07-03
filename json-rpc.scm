(define-module (json-rpc)
  #:use-module (web http)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 iconv)
  #:use-module (json)

  #:export (<response>
            make-response
            response?
            response-id
            response-result
            response-error

            <request>
            make-request
            request?
            request-id
            request-method
            request-params

            readMessage
            sendMessage
            sendResult
            sendError
            sendNotification

            ParseError
            InvalidRequest
            MethodNotFound
            InvalidParams
            InternalError
            ServerErrorStart
            ServerErrorEnd
            ServerNotInitialized
            UnknownErrorCode))

(define-record-type <response>
  (make-response id result error)
  response?
  (id response-id)
  (result response-result)
  (error response-error))

(define-record-type <request>
  (make-request id method params)
  request?
  (id request-id)
  (method request-method)
  (params request-params))

(define (readToCRNL port)
  (define (aux acc)
    (define b (get-u8 port))
    (if (and (= (char->integer #\cr) b)
             (= (char->integer #\lf) (lookahead-u8 port)))
      (begin
        (get-u8 port) ;; Consume \n
        (bytevector->string (u8-list->bytevector (reverse acc)) "ascii"))
      (aux (cons b acc))))
  (aux '()))

(define (readHeaders port)
  (define (aux acc)
    (define line (readToCRNL port))
    (if (equal? line "")
      acc
      (let* ((i (string-index line #\:))
             (key (string-take line i))
             (value (string-drop line (+ i 2))))
        (aux (cons (cons key value) acc)))))
  (aux '()))

(define (getContentLength headers)
  (string->number (assoc-ref headers "Content-Length")))

(define (getContentEncoding headers)
  (define header (assoc-ref headers "Content-Type"))
  (define encoding
    (if (eq? header #f)
      "utf-8"
      (or (assoc-ref (cdr (parse-header 'content-type header)) 'charset)
          "utf-8")))
  ;; for backwards compatibility treat utf8 as utf-8
  (if (equal? encoding "utf8") "utf-8" encoding))

(define (readContent headers port)
  (define len (getContentLength headers))
  (define encoding (getContentEncoding headers))
  (bytevector->string (get-bytevector-n port len) encoding))

(define (parseContent content)
  (define root (json-string->scm content))
  ;; TODO verify jsonrpc version
  (if (eq? (hash-ref root "method") #f)
    (make-response
     (hash-ref root "id")
     (hash-ref root "result")
     (hash-ref root "error"))
    (make-request
     (hash-ref root "id")
     (hash-ref root "method")
     (hash-ref root "params"))))

(define (readMessage port)
  (define headers (readHeaders port))
  (define content (readContent headers port))
  (define tmp (parseContent content))
  (display tmp) (display "\n")
  ;(parseContent content))
  tmp)

(define (sendMessage port response)
  (define json (cons '(jsonrpc . "2.0") response))
  (define encoding "utf-8")
  (define body (string->bytevector (scm->json-string json) encoding))
  (define header-string
    (string-append
     "Content-Length: " (number->string (bytevector-length body)) "\r\n"
     "Content-Type: application/vscode-jsonrpc; charset=" encoding "\r\n"
     "\r\n"))
  (define header (string->bytevector header-string "ascii"))
  (put-bytevector port header)
  (put-bytevector port body)
  (force-output port))

(define (sendNotification port method params)
  (sendMessage port `((method . ,method) (params . ,params))))

;; Defined by JSON RPC
(define ParseError -32700)
(define InvalidRequest -32600)
(define MethodNotFound -32601)
(define InvalidParams -32602)
(define InternalError -32603)
(define ServerErrorStart -32099)
(define ServerErrorEnd -32000)
(define ServerNotInitialized -32002)
(define UnknownErrorCode -32001)

(define* (sendError port requestId errorId errorMessage #:optional (data #nil))
  (define error
    `((code . ,errorId)
      (message . ,errorMessage)
      ,@(if (eq? data #nil) '() `(data . ,data))))
  (sendMessage port `((id . ,requestId) (error . ,error))))

(define (sendResult port requestId result)
  (sendMessage port `((id . ,requestId) (result . ,result))))
