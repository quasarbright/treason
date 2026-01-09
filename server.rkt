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

;; TODO headers in ascii body in utf-8?

(define (main [in (current-input-port)] [out (current-output-port)])
  (let loop ()
    (define msg (read-message in))
    (match msg
      [(request id method parameters)
       (write-message (response id "foo" 'null) out)]
      [(notification method parameters)
       (void)])))

;; -> message?
(define (read-message [in (current-input-port)])
  (read-line in 'return-linefeed)
  (read-line in 'return-linefeed)
  (define js (read-json in))
  (cond
    [(and (hash-has-key? js 'id)
          (hash-has-key? js 'method)
          (hash-has-key? js 'params))
     (request (hash-ref js 'id)
              (string->symbol (hash-ref js 'method))
              (hash-ref js 'params))]
    [(and (hash-has-key? js 'method)
          (hash-has-key? js 'params))
     (notification (string->symbol (hash-ref js 'method))
                   (hash-ref js 'params))]
    [(and (hash-has-key? js 'id)
          (or (hash-has-key? js 'result)
              (hash-has-key? js 'error)))
     (response (hash-ref js 'id)
               (hash-ref js 'result 'null)
               (hash-ref js 'error 'null))]))

(module+ test
  ;; read-message tests
  (check-equal? (read-message (open-input-string "blah\r\n\r\n{\"jsonrpc\": \"2.0\", \"id\": 1, \"method\": \"foo\", \"params\": []}"))
                (request 1 'foo '()))
  
  (check-equal? (read-message (open-input-string "blah\r\n\r\n{\"jsonrpc\": \"2.0\", \"method\": \"bar\", \"params\": {}}"))
                (notification 'bar '#hasheq()))
  
  (check-equal? (read-message (open-input-string "blah\r\n\r\n{\"jsonrpc\": \"2.0\", \"id\": 2, \"result\": \"success\"}"))
                (response 2 "success" 'null))

  (check-equal? (read-message (open-input-string "blah\r\n\r\n{\"jsonrpc\": \"2.0\", \"id\": 2, \"error\": \"failure\"}"))
                (response 2 'null "failure")))

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
    (define len (string-length str))
    (display (format "Content-Length: ~a\r\n\r\n~a" len str)))

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
                "Content-Length: 42\r\n\r\n{\"error\":\"failure\",\"id\":2,\"jsonrpc\":\"2.0\"}"))