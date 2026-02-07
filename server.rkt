#lang racket

;; Language server
;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/

(provide)
(module+ main (main))
(module+ test (require rackunit))
(require json
         "stx.rkt"
         "stx-quote.rkt"
         "reader.rkt"
         "constants.rkt"
         "server-logging.rkt"
         "expander.rkt")

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
  (define client (new proxy-client% [in in] [out out]))
  (define srv (new server% [client client]))
  (log-server-info "started server")
  (let loop ()
    (match (read-message in)
      [(request id method parameters)
       (define rsp
         (if (object-method-arity-includes? srv method 1)
             (with-handlers ([exn:fail? (lambda (err) (log-server-error "~a" err) (response id 'null (hasheq 'code -32603 'message (format "internal error: ~a" err))))])
               (define data (dynamic-send srv method parameters))
               (response id data 'null))
             (response id 'null (hasheq 'code -32601 'message (format "Unknown method: ~a" method)))))
       (write-message rsp out)]
      [(notification method parameters)
       (when (object-method-arity-includes? srv method 1)
         (with-handlers ([exn:fail? (lambda (err) (log-server-error "~a" err))])
           (dynamic-send srv method parameters)))])
    (loop)))

(define proxy-client%
  (class object%
    (super-new)
    (init-field [in (current-input-port)]
                [out (current-output-port)])

    (define next-id 0)
    
    (define-syntax-rule (define-remote/notification method-name)
      (define/public (method-name params)
        (write-message (notification 'method-name params) out)))
    
    (define-remote/notification textDocument/publishDiagnostics)))

(define server%
  (class object%
    (super-new)

    ;; maps uris to text documents
    (define text-documents (make-hash))

    (init-field client)

    (define/public (initialize parameters)
      ;; TODO record client capabilities
      (hasheq 'capabilities
              (hasheq 'textDocumentSync (hasheq 'openClose #t
                                                'change TextDocumentSyncKind/Full)
                      'documentSymbolProvider #t
                      'definitionProvider #t
                      'referencesProvider #t)))

    (define/public (textDocument/documentSymbol parameters)
      (match parameters
        [(hash* ['textDocument (hash* ['uri uri])])
         (define text-document (hash-ref text-documents uri (lambda () (error 'textDocument/documentSymbol "unknown document ~a" uri))))
         (define stx (string->stx uri (hash-ref text-document 'text)))
         (define stx^ (expand stx))
         (get-document-symbols stx^)]))
    
    (define/public (textDocument/definition parameters)
      (match parameters
        [(hash* ['textDocument (hash* ['uri uri])]
                ['position pos])
         (define text-document (hash-ref text-documents uri (lambda () (error 'textDocument/documentSymbol "unknown document ~a" uri))))
         (define stx (string->stx uri (hash-ref text-document 'text)))
         (define stx^ (expand stx))
         (define result (goto-definition stx^ (position->loc pos uri)))
         (if result
             (span->location (stx-span result))
             'null)]))
    
    (define/public (textDocument/references parameters)
      (match parameters
        [(hash* ['textDocument (hash* ['uri uri])]
                ['position pos])
         (define text-document (hash-ref text-documents uri (lambda () (error 'textDocument/documentSymbol "unknown document ~a" uri))))
         (define stx (string->stx uri (hash-ref text-document 'text)))
         (define stx^ (expand stx))
         (for/list ([id (find-references stx^ (position->loc pos uri))])
           (span->location (stx-span id)))]))
    
    (define/public (textDocument/didOpen parameters)
      (define text-document (hash-ref parameters 'textDocument))
      (define uri (hash-ref text-document 'uri))
      (hash-set! text-documents uri text-document)
      (push-diagnostics uri))
    
    (define/private (push-diagnostics uri)
      (define text-document
        (hash-ref text-documents
                  uri
                  (lambda () (error 'textDocument/didChange "unknown document: ~a" uri))))
      (define stx (string->stx uri (hash-ref text-document 'text)))
      (define stx^ (expand stx))
      (define errors (find-stx-errors stx^))
      (define diagnostics (map stx-error->diagnostic errors))
      (send client textDocument/publishDiagnostics 
            (hasheq 'uri uri
                    'diagnostics diagnostics)))

    (define/public (textDocument/didChange parameters)
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
         (hash-set! text-documents uri new-text-document)
         (push-diagnostics uri)]))
    
    (define/public (textDocument/didClose parameters)
      (match parameters
        [(hash* ['textDocument (hash* ['uri uri])])
         (hash-remove! text-documents uri)]))))

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

;; stx? -> (listof (hash 'name string? 'kind SymbolKind 'location Location))
(define (get-document-symbols syn)
  (match syn
    [(stx-quote (let ([,(stx x x-span) ,rhs]) ,body))
     (cons (hash 'name (symbol->string x)
                 'kind SymbolKind/Variable
                 'location (span->location x-span))
           (append-map get-document-symbols (list rhs body)))]
    [(stx-quote (if ,cnd ,thn ,els))
     (append (get-document-symbols cnd)
             (get-document-symbols thn)
             (get-document-symbols els))]
    [(stx-quote (let-syntax-rule ([,_ ,_]) ,body))
     ;; if we see this in expanded code, it means the pattern is malformed and defines nothing
     (get-document-symbols body)]
    [(stx (cons _ _) _)
     ;; shouldn't see this in expanded code
     (list)]
    [_ (list)]))

;; stx? loc? -> (or #f stx?)
;; takes in program and a location of a reference
;; returns binding identifier or #f if there is none
(define (goto-definition syn lc)
  (let loop ([syn syn]
             ; maps binding symbol to binding identifier
             [bindings (hash)])
    (match syn
      [(stx-quote (let ([,(and x-syn (stx x x-span)) ,rhs]) ,body))
       (cond
         ;; goto-definition on the binding site returns itself
         [(loc-in-span? lc x-span)
          x-syn]
         [else
          (define rhs-result (loop rhs bindings))
          (or rhs-result
              ;; assume x is symbol
              (loop body (hash-set bindings x x-syn)))])]
      [(stx-quote (if ,cnd ,thn ,els))
       (or (loop cnd bindings)
           (loop thn bindings)
           (loop els bindings))]
      [(stx-quote (let-syntax-rule ([,_ ,_]) ,body))
       ;; shouldn't see this in an expanded program unless pattern expansion failed,
       ;; which means this isn't a valid pattern and thus defines nothing
       (loop body bindings)]
      [(stx (cons _ _) _)
       ;; application (macro invocation)
       ;; shouldn't see this in expanded code ever, it'll just be a stx-error
       #f]
      [(stx (? symbol? id) spn)
       (and (loc-in-span? lc spn)
            (hash-ref bindings id #f))]
      [_ #f])))

(module+ test
  (let ([syn (string->stx "test.tsn" "(let ([x 2]) x)")])
    (check-equal? (goto-definition syn (loc "test.tsn" 0 13))
                  (stx 'x (span (loc "test.tsn" 0 7) (loc "test.tsn" 0 8))))
    ;; loc is the binding site
    (check-equal? (goto-definition syn (loc "test.tsn" 0 7))
                  (stx 'x (span (loc "test.tsn" 0 7) (loc "test.tsn" 0 8))))
    ;; loc is a space
    (check-equal? (goto-definition syn (loc "test.tsn" 0 12))
                  #f)
    ;; loc is not an identifier
    (check-equal? (goto-definition syn (loc "test.tsn" 0 9))
                  #f)
    ;; loc is an unbound identifier
    (check-equal? (goto-definition (string->stx "test.tsn" "(let ([x 2]) y)")
                                   (loc "test.tsn" 0 13))
                  #f)
    ;; shadowing
    (check-equal? (goto-definition (string->stx "test.tsn" "(let ([x 2]) (let ([x 3]) x))")
                                   (loc "test.tsn" 0 26))
                  (stx 'x (span (loc "test.tsn" 0 20) (loc "test.tsn" 0 21))))
    ;; shadowing binding site
    (check-equal? (goto-definition (string->stx "test.tsn" "(let ([x 2]) (let ([x 3]) x))")
                                   (loc "test.tsn" 0 20))
                  (stx 'x (span (loc "test.tsn" 0 20) (loc "test.tsn" 0 21))))
    ;; malformed rhs shouldn't get in the way
    (check-equal? (goto-definition (string->stx "test.tsn" "(let ([x (let)]) x)")
                                   (loc "test.tsn" 0 17))
                  (stx 'x (span (loc "test.tsn" 0 7) (loc "test.tsn" 0 8))))
    ;; macro around let, maintain use-site spans
    (check-equal? (goto-definition (expand (string->stx "test.tsn" "(let-syntax-rule ([(mylet ([x rhs]) body) (let ([x rhs]) body)]) (mylet ([x 1]) x))"))
                                   (loc "test.tsn" 0 80))
                  (stx 'x (span (loc "test.tsn" 0 74) (loc "test.tsn" 0 75))))
    ;; (unhygienic) macro-introduced binding, definition is in template
    (check-equal? (goto-definition (expand (string->stx "test.tsn" "(let-syntax-rule ([(let-x body) (let ([x 1]) body)]) (let-x x))"))
                                   (loc "test.tsn" 0 60))
                  (stx 'x (span (loc "test.tsn" 0 39) (loc "test.tsn" 0 40))))))

;; stx? loc? -> (or #f (listof stx?))
;; #f when it can't even find the binder
(define (find-references syn lc)
  (let loop ([syn syn])
    (match syn
      [(stx-quote (let ([,(stx x x-span) ,rhs]) ,body))
       (if (loc-in-span? lc x-span)
           ;; found the binder, now collect references in body
           (find-references/help body x)
           (or (loop rhs)
               (loop body)))]
      [(stx-quote (if ,cnd ,thn ,els))
       (or (loop cnd)
           (loop thn)
           (loop els))]
      [(stx-quote (let-syntax-rule ([,_ ,_]) ,body))
       ;; shouldn't see this in an expanded program unless pattern expansion failed,
       ;; which means this isn't a valid pattern and thus defines nothing
       (loop body)]
      [(stx (cons _ _) _)
       ;; application (macro invocation)
       ;; shouldn't see this in expanded code ever, it'll just be a stx-error
       #f]
      [_ #f])))

(module+ test
  (check-equal? (find-references (string->stx "test.tsn" "(let ([x 2]) x)")
                                 (loc "test.tsn" 0 7))
                (list (stx 'x (span (loc "test.tsn" 0 13) (loc "test.tsn" 0 14)))))
  ;; shadowing, don't include unbound
  (check-equal? (find-references (string->stx "test.tsn" "(let ([x x]) (let ([x x]) x))")
                                 (loc "test.tsn" 0 7))
                (list (stx 'x (span (loc "test.tsn" 0 22) (loc "test.tsn" 0 23)))))
  ;; multiple references
  (check-equal? (find-references (string->stx "test.tsn" "(let ([x 2]) (let ([y x]) x))")
                                 (loc "test.tsn" 0 7))
                (list (stx 'x (span (loc "test.tsn" 0 22) (loc "test.tsn" 0 23)))
                      (stx 'x (span (loc "test.tsn" 0 26) (loc "test.tsn" 0 27)))))
  ;; macro around let, maintain use-site spans
  (check-equal? (find-references (expand (string->stx "test.tsn" "(let-syntax-rule ([(mylet ([x rhs]) body) (let ([x rhs]) body)]) (mylet ([x 1]) x))"))
                                 (loc "test.tsn" 0 74))
                (list (stx 'x (span (loc "test.tsn" 0 80) (loc "test.tsn" 0 81)))))
  ;; (unhygienic) macro-introduced reference gets definition-site spans, which are the same for each macro use.
  ;; strange, but this will go away with hygiene. also, vscode dedups
  (check-equal? (find-references (expand (string->stx "test.tsn" "(let-syntax-rule ([(m) x]) (let ([x 1]) (let ([y (m)]) (m))))"))
                                 (loc "test.tsn" 0 34))
                ;; duplicated spans
                (list (stx 'x (span (loc "test.tsn" 0 23) (loc "test.tsn" 0 24)))
                      (stx 'x (span (loc "test.tsn" 0 23) (loc "test.tsn" 0 24))))))

;; stx? symbol? -> (listof stx?)
;; find references of x in syn
(define (find-references/help syn x)
  (match syn
    [(stx-quote (let ([,(stx y _) ,rhs]) ,body))
     (append (find-references/help rhs x)
             (if (eq? x y)
                 ;; shadowed, no more references
                 (list)
                 (find-references/help body x)))]
    [(stx-quote (if ,cnd ,thn ,els))
     (append (find-references/help cnd x)
             (find-references/help thn x)
             (find-references/help els x))]
    [(stx-quote (let-syntax-rule ([,_ ,_]) ,body))
     ;; don't consider references in template because you have no idea where they'll end up.
     ;; shouldn't see this in an expanded program unless pattern expansion failed
     (find-references/help body x)]
    [(stx (cons _ _) _)
     ;; application (macro invocation)
     ;; shouldn't see this in expanded code ever, it'll just be a stx-error
     (list)]
    [(stx (? symbol? y) _)
     (if (eq? x y)
         (list syn)
         (list))]
    [_ (list)]))

;; loc? span? -> boolean?
;; is the location contained within the span?
(define (loc-in-span? lc spn)
  (match spn
    [(span lc-start lc-end)
     (and (loc<=? lc-start lc)
          (loc<? lc lc-end))]))

;; is lc1 before or on lc2?
(define (loc<=? lc1 lc2)
  (or (equal? lc1 lc2)
      (loc<? lc1 lc2)))

;; is lc1 before lc2?
(define (loc<? lc1 lc2)
  (match* (lc1 lc2)
    [((loc src1 line1 col1) (loc src2 line2 col2))
     (and (equal? src1 src2)
          (if (= line1 line2)
              (< col1 col2)
              (< line1 line2)))]))

(define (span->location spn)
  (hash 'uri (loc-source (span-start spn))
        'range (span->range spn)))

(define (span->range spn)
  (hash 'start (loc->position (span-start spn))
        'end (loc->position (span-end spn))))

(define (loc->position lc)
  (hash 'line (loc-line lc) 'character (loc-column lc)))

(define (position->loc pos source)
  (match pos
    [(hash* ['line line] ['character col])
     (loc source line col)]))

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

;; stx-error? -> hasheq?
(define (stx-error->diagnostic err)
  (hasheq 'range (span->range ( stx-error-span err))
          'severity DiagnosticSeverity/Error
          'source "treason"
          'message (stx-error-diagnostic-message err)))

;; stx-error? -> string?
(define (stx-error-diagnostic-message err)
  (match err
    [(stx-error #f message _ _)
     message]
    [(stx-error who message _ _)
     (format "~a: ~a" who message)]))

;; stx-error? -> span?
;; where should the red squiggly go?
(define (stx-error-span err)
  (match err
    [(stx-error _ _ expr #f)
     (stx-span expr)]
    [(stx-error _ _ _ sub-expr)
     (stx-span sub-expr)]))