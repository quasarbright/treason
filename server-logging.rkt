#lang racket

;; logging utilities for the language server

(provide log-server-debug
         log-server-info
         log-server-warning
         log-server-error
         log-server-fatal)
(require racket/runtime-path)

;; Logging setup
(define-logger server)

;; TODO send them to the root of the lsp project? this file may end up in private
(define-runtime-path log-file-path "logs.txt")

;; Format a log event with timestamp, level, location, and message
(define (format-log-event level message data)
  (define now (seconds->date (current-seconds)))
  (define timestamp (format "~a/~a/~a ~a:~a:~a~a"
                            (date-year now)
                            (~r (date-month now) #:min-width 2 #:pad-string "0")
                            (~r (date-day now) #:min-width 2 #:pad-string "0")
                            (~r (if (> (date-hour now) 12) (- (date-hour now) 12) (date-hour now)) #:min-width 2 #:pad-string "0")
                            (~r (date-minute now) #:min-width 2 #:pad-string "0")
                            (~r (date-second now) #:min-width 2 #:pad-string "0")
                            (if (>= (date-hour now) 12) "pm" "am")))
  (define location 
    (if (continuation-mark-set? data)
        (let ([context (continuation-mark-set->context data)])
          (if (and (pair? context) (cdr (car context)))
              (let ([srcloc (cdr (car context))])
                (format "~a:~a" 
                        (let ([src (srcloc-source srcloc)])
                          (if (path? src)
                              (file-name-from-path src)
                              src))
                        (srcloc-line srcloc)))
              "unknown"))
        "unknown"))
  (format "[~a]\t[~a]\t[~a]\t~a\n" timestamp location level message))

;; Start log receiver in background thread
(define (start-log-receiver log-file-path)
  (define log-port (open-output-file log-file-path #:exists 'append))
  (define receiver (make-log-receiver server-logger 'debug))
  
  (thread
   (lambda ()
     (let loop ()
       (define event (sync receiver))
       (match-define (vector level message data topic) event)
       (define formatted (format-log-event level message data))
       (display formatted log-port)
       (flush-output log-port)
       (loop)))))

;; Initialize logging
(define log-receiver-thread (start-log-receiver log-file-path))