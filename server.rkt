#lang racket

;; Language server
;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/

(provide)
(module+ main (main))
(module+ test (require rackunit))
(require json)

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

(define (main [in (current-input-port)] [out (current-output-port)])
  (let loop ()
    (define msg (read-message in))
    (match msg
      [(request id method parameters)
       (write-message (response id "foo" 'null) out)]
      [(notification method parameters)
       (void)])
    (loop)))

;; -> message?
(define (read-message [in (current-input-port)])
  (define headers (read-headers in))
  (define content-length (string->number (hash-ref headers 'Content-Length (lambda () (error 'read-message "missing Content-Length header")))))
  (define bytes (read-bytes content-length in))
  (define js-str (bytes->string/utf-8 bytes))

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