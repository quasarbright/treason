#lang racket

;; Language server
;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/

(provide)
(module+ main (main))
(module+ test (require rackunit))
(require json
         "stx.rkt"
         "reader.rkt"
         "constants.rkt"
         "server-logging.rkt")

;; An Request is a
(struct request [id method params] #:transparent)
;; where
;; id is a Natural
;; method is a symbol like 'textDocument/completion
;; params is a (U (Listof JSExpr) (Hash Symbol JSExpr))
;; Represents a call that needs a response

;; A Response is a
(struct response [id result error] #:transparent)
;; where
;; id is a Natural (the same as its corresponding request)
;; result is a JSExpr
;; error is a JSExpr. 'null if and only if result is present
;; Represents the response to a Request

;; A Notification is a
(struct notification [method params] #:transparent)
;; where
;; id is a Natural
;; method is a symbol like 'textDocument/completion
;; params is a (U (Listof JSExpr) (Hash Symbol JSExpr))
;; Represents a call that does not need a response

;; A Message is one of
;; A Request
;; A Response
;; A Notification

#|
idea for abstraction
- server class with class methods for lsp requests/notifications. each would take in params and return either void (for notifications) or json (for response)
- server just dispatches using dynamic-send
- server will eventually need a client for diagnostic reporting. will be a proxy. can test with a mock
- server can initially be sync but client will need to be async and do continuation stuff if you need a response. then the server will need to be async so it doesn't block
- for async stuff, have a pool of pending requests keyed by id, each with a continuation for resuming upon response

not sure hot to do automatic capabilities
do we want actual types instead of json?
|#

(define (main [in (current-input-port)] [out (current-output-port)])
  (log-server-info "started server")
  (let loop ()
    (match (read-message in)
      [(request id method parameters)
       (match method
         ['initialize
          (write-message (response id (hasheq 'capabilities
                                                           (hasheq 'textDocumentSync
                                                                   (hasheq 'openClose #t
                                                                           'change TextDocumentSyncKind/Full)
                                                                   'documentSymbolProvider #t))
                                                   'null))]
         ['textDocument/documentSymbol
          (match parameters
            [(hash* ['textDocument (hash* ['uri uri])])
             (define text-document (hash-ref text-documents uri (lambda () (error 'textDocument/documentSymbol "unknown document ~a" uri))))
             (define stx (string->stx uri (hash-ref text-document 'text)))
             (write-message (response id (get-document-symbols stx) 'null))])]
         [_
          (write-message (response id 'null (hasheq 'code -32601 'message (format "unknown method: ~a" method)))
                         out)])]
      [(notification method parameters)
       (match method
         ['textDocument/didOpen
          (define text-document (hash-ref parameters 'textDocument))
          (define uri (hash-ref text-document 'uri))
          (hash-set! text-documents uri text-document)]
         ['textDocument/didChange
          (match parameters
            [(hash*
              ['textDocument (hash* ['version version] ['uri uri])]
              ['contentChanges changes])
             (define text-document
               (hash-ref text-documents
                         uri
                         (lambda () (error 'textDocument/didChange "unknown document: ~a" uri))))
             (define old-text (hash-ref text-document 'text))
             (define new-text
               (for/fold ([new-text old-text])
                         ([change changes])
                 (match change
                   [(hash* ['text text] ['range range #:default #f])
                    (match range
                        [(hash* ['start (hash* ['line start-line] ['character start-col])]
                                ['end (hash* ['line end-line] ['character end-col])])
                         (define start-index (position->index new-text start-line start-col))
                         (define dest (string-copy new-text))
                         (string-copy! dest text start-index)]
                        [_ text])])))
             (define new-text-document
               (hash-set (hash-set text-document 'text new-text)
                         'version version))
             (hash-set! text-documents uri new-text-document)])]
         ['textDocument/didClose
          (match parameters
            [(hash* ['textDocument (hash* ['uri uri])])
             (hash-remove! text-documents uri)])]
         [_ (void)])])
    (loop)))

;; maps uris to text documents
(define text-documents (make-hash))

;; string? natural? natural? -> natural?
;; computes the index corresponding to the given position in the text
;; line and col are 0-indexed
(define (position->index text line col)
  (let loop ([idx 0] [current-line 0])
    (cond
      [(= current-line line)
       ;; assumes col doesn't go past end of line
       (+ idx col)]
      [(>= idx (string-length text))
       idx]
      [(char=? (string-ref text idx) #\newline)
       (loop (add1 idx) (add1 current-line))]
      [else
       (loop (add1 idx) current-line)])))

(module+ test
  (check-equal? (position->index "hello" 0 0) 0)
  (check-equal? (position->index "hello" 0 5) 5)
  (check-equal? (position->index "hello\nworld" 0 0) 0)
  (check-equal? (position->index "hello\nworld" 1 0) 6)
  (check-equal? (position->index "hello\nworld" 1 5) 11)
  (check-equal? (position->index "line1\nline2\nline3" 2 3) 15))

;; stx? -> jsexpr?
(define (get-document-symbols syn)
  (match syn
    [(stx (list syn ...) _)
     (append-map get-document-symbols syn)]
    [(stx (? symbol? id) spn)
     (list (hash 'name (symbol->string id)
                 'kind SymbolKind/Variable
                 'location (span->location spn)))]
    [_ (list)]))

(define (span->location spn)
  (hash 'uri (loc-source (span-start spn))
        'range (span->range spn)))

(define (span->range spn)
  (hash 'start (loc->position (span-start spn))
        'end (loc->position (span-end spn))))

(define (loc->position lc)
  (hash 'line (loc-line lc) 'character (loc-column lc)))

;; -> message?
(define (read-message [in (current-input-port)])
  (define headers (read-headers in))
  (define content-length (string->number (hash-ref headers 'Content-Length (lambda () (error 'read-message "missing Content-Length header")))))
  (define bytes (read-bytes content-length in))
  (define js-str (bytes->string/utf-8 bytes))
  (log-server-debug "received message: ~a" js-str)
  (define js (string->jsexpr js-str))
  (match js
    [(hash* ['id id] ['method method] ['params params])
     (request id
              (string->symbol method)
              params)]
    [(hash* ['method method] ['params params])
     (notification (string->symbol method)
                   params)]
    [(hash* ['id id] ['result result])
     (response id result 'null)]
    [(hash* ['id id] ['error error])
     (response id 'null error)]))

(module+ test
  (define-check (check-read-of-write msg)
    (let ([msg-v msg])
      (check-equal? (read-message (open-input-string (with-output-to-string (lambda () (write-message msg-v)))))
                    msg-v)))
  ;; read-message tests
  (check-read-of-write (request 1 'foo '()))
  (check-read-of-write (notification 'bar '#hasheq()))
  (check-read-of-write (response 2 "success" 'null))
  (check-read-of-write (response 2 'null "failure"))

  ;; multiple header lines
  (check-equal? (read-message (open-input-string "Content-Length: 58\r\nContent-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\n{\"jsonrpc\": \"2.0\", \"id\": 2, \"error\": \"failure\"}"))
                (response 2 'null "failure"))
  ;; 2 reads
  (let ([in (open-input-string "Content-Length: 47\r\n\r\n{\"jsonrpc\": \"2.0\", \"id\": 2, \"error\": \"failure\"}Content-Length: 47\r\n\r\n{\"jsonrpc\": \"2.0\", \"id\": 2, \"error\": \"failure\"}")])
    (check-equal? (read-message in)
                  (response 2 'null "failure"))
    (check-equal? (read-message in)
                  (response 2 'null "failure")))
  ;; 2 reads with newline in between. works bc content length
  (let ([in (open-input-string "Content-Length: 48\r\n\r\n{\"jsonrpc\": \"2.0\", \"id\": 2, \"error\": \"failure\"}\nContent-Length: 47\r\n\r\n{\"jsonrpc\": \"2.0\", \"id\": 2, \"error\": \"failure\"}")])
    (check-equal? (read-message in)
                  (response 2 'null "failure"))
    (check-equal? (read-message in)
                  (response 2 'null "failure"))))

;; -> (Hash Symbol String)
(define (read-headers [in (current-input-port)])
  (let loop ([headers (hash)])
    (define line (read-line in 'return-linefeed))
    (define result (regexp-match #px"([^:]+): (.*)" line))
    (match result
      [(list _ name value)
       (loop (hash-set headers (string->symbol name)
                               value))]
      [#f headers])))

(module+ test
  (check-equal? (read-headers (open-input-string "Content-Length: 2\r\nContent-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\n{}"))
                (hash 'Content-Length "2" 'Content-Type "application/vscode-jsonrpc; charset=utf-8")))

;; message? -> void?
(define (write-message msg [out (current-output-port)])
  (define js
    (match msg
      [(request id method params)
       (hash 'jsonrpc "2.0"
             'id id
             'method (symbol->string method)
             'params params)]
      [(response id result error)
       (if (eq? error 'null)
           (hash 'jsonrpc "2.0"
                 'id id
                 'result result)
           (hash 'jsonrpc "2.0"
                 'id id
                 'error error))]
      [(notification method params)
       (hash 'jsonrpc "2.0"
             'method (symbol->string method)
             'params params)]))
  (define str (jsexpr->string js))
  (log-server-debug "writing message ~a" str)
  (define bytes (string->bytes/utf-8 str))
  (define len (bytes-length bytes))
  (display (format "Content-Length: ~a\r\n\r\n~a" len str))
  (flush-output out))

(module+ test
  (check-equal? (with-output-to-string
                  (lambda () (write-message (request 1 'foo '()))))
                "Content-Length: 51\r\n\r\n{\"id\":1,\"jsonrpc\":\"2.0\",\"method\":\"foo\",\"params\":[]}")
  
  (check-equal? (with-output-to-string
                  (lambda () (write-message (notification 'bar '#hash()))))
                "Content-Length: 44\r\n\r\n{\"jsonrpc\":\"2.0\",\"method\":\"bar\",\"params\":{}}")
  
  (check-equal? (with-output-to-string
                  (lambda () (write-message (response 2 "success" 'null))))
                "Content-Length: 43\r\n\r\n{\"id\":2,\"jsonrpc\":\"2.0\",\"result\":\"success\"}")
  (check-equal? (with-output-to-string
                  (lambda () (write-message (response 2 'null "failure"))))
                "Content-Length: 42\r\n\r\n{\"error\":\"failure\",\"id\":2,\"jsonrpc\":\"2.0\"}")
  ;; bytes content length
  (check-equal? (with-output-to-string
                  (lambda () (write-message (response 2 'null "😎"))))
                "Content-Length: 39\r\n\r\n{\"error\":\"😎\",\"id\":2,\"jsonrpc\":\"2.0\"}"))