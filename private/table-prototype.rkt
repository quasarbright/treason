#lang errortrace racket

;; prototype for having analysis information in tables rather than in stx objects.
;; we can analyze once and then just fetch data from tables.
;; but we need to re-analyze the whole file on every change.
;; query-based with reactivity and memoization would be even better, but more complex.

;; type inference and reference + definition information

(module+ test (require rackunit))
(require (for-syntax syntax/parse))

(struct stx [node-id e] #:transparent)
;; where
;; node-id is a Natural that is unique
;; e is either an atom or (listof stx?)

;; a Type is one of
;; 'number
;; 'boolean

;; a StaticError is one of
(struct unbound-var [node-id] #:transparent)
(struct type-mismatch [node-id expected-type actual-type] #:transparent)
(define static-error? (or/c unbound-var? type-mismatch?))

;; state

(define next-node-id 0)
(define (new-node-id!)
  (begin0 next-node-id
    (set! next-node-id (add1 next-node-id))))

;; stx info, mapping node ids to data

;; map expr to type or #f (when inference fails)
(define types (make-hasheq))
;; map reference site to definition site node id
(define definitions (make-hasheq))
;; map definition site to list of reference site node ids
(define references (make-hasheq))
;; map node id to its stx
(define exprs (make-hasheq))
;; map expr to parent node id
(define parents (make-hasheq))
;; map expr to list of static errors associated with it
(define errors (make-hasheq))
;; natural? static-error? -> void?
(define (record-static-error! node-id err)
  (hash-set! errors node-id (cons err (hash-ref errors node-id (list)))))

;; -> void?
(define (reset!)
  (set! next-node-id 0)
  (hash-clear! types)
  (hash-clear! definitions)
  (hash-clear! references)
  (hash-clear! exprs)
  (hash-clear! parents)
  (hash-clear! errors))

;; stx? -> void?
(define (analyze! syn)
  (analyze-references! syn)
  (analyze-types! syn))

(module+ test
  (reset!)
  (check-equal? (stx-quote x)
                (stx 0 'x))
  (reset!)
  (check-equal? (stx-quote (a (b c)))
                (stx 0 (list (stx 1 'a) (stx 2 (list (stx 3 'b) (stx 4 'c))))))
  (reset!)
  (analyze! (stx-quote x))
  (check-equal? (hash-ref errors 0) (list (unbound-var 0)))
  (check-equal? (hash-ref types 0) #f)

  (reset!)
  (analyze! (stx-quote (let ([x 1]) (if x x x))))
  (check-true (hash-empty? errors))
  (check-equal? (hash-ref references 4) (list 10 9 8))
  (check-equal? (hash-ref definitions 4) 4)
  (check-equal? (hash-ref definitions 8) 4)
  (check-equal? (hash-ref definitions 9) 4)
  (check-equal? (hash-ref definitions 10) 4)
  (check-equal? (hash-ref types 0) 'number)
  
  (reset!)
  (analyze! (stx-quote (let ([x 1]) (let ([x 2]) (if x x x)))))
  (check-true (hash-empty? errors))
  (check-equal? (hash-ref references 4 (list)) (list))
  (check-equal? (hash-ref definitions 4) 4)
  (check-equal? (hash-ref references 10) (list 16 15 14))
  (check-equal? (hash-ref definitions 10) 10)
  (check-equal? (hash-ref definitions 14) 10)
  (check-equal? (hash-ref definitions 15) 10)
  (check-equal? (hash-ref definitions 16) 10))

(define-match-expander stx-quote
  (syntax-parser
    [(_ datum)
     (let loop ([datum #'datum])
       (syntax-parse datum
         #:datum-literals (stx-unquote unquote)
         [(stx-unquote pat) #'pat]
         [(unquote pat) #'pat]
         [(datum ...)
          #`(stx _ (list #,@(map loop (attribute datum))))]
         [atom #'(? (stx-eq-to-datum? 'atom))]))])
  (syntax-parser
    ;; TODO unquote
    [(_ datum)
     #'(datum->stx 'datum)]))

;; any/c -> (stx? -> boolean?)
(define ((stx-eq-to-datum? datum) syn)
  (eq? datum (stx->datum syn)))

(define (datum->stx datum)
  (match datum
    [(? list? datums)
     (define node-id (new-node-id!))
     (define children (map datum->stx datums))
     (for ([child children])
       (hash-set! parents (stx-node-id child) node-id))
     (define syn (stx node-id children))
     (hash-set! exprs node-id syn)
     syn]
    [atom
     (stx (new-node-id!) atom)]))

(define (stx->datum syn)
  (match syn
    [(stx _ (? list? es))
     (map stx->datum es)]
    [(stx _ atom) atom]))

;; stx? [(hasheq symbol? natural?)] -> void?
;; env maps name to binding site node id
;; analyze definitions and references for stuff like goto-definition
(define (analyze-references! syn [env (hasheq)])
  (match syn
    [(stx-quote (let ([,(stx x-id (? symbol? x)) ,rhs]) ,body))
     (analyze-references! rhs env)
     (hash-set! definitions x-id x-id)
     (analyze-references! body (hash-set env x x-id))]
    [(stx-quote (if ,cnd ,thn ,els))
     (analyze-references! cnd env)
     (analyze-references! thn env)
     (analyze-references! els env)]
    [(stx reference-id (? symbol? x))
     (define definition-id (hash-ref env x #f))
     (cond
       [definition-id
        (hash-set! definitions reference-id definition-id)
        ;; assuming there is already an entry
        ;; since definition site recorded itself as a reference
        (hash-set! references definition-id (cons reference-id (hash-ref references definition-id (list))))]
       [else (record-static-error! reference-id (unbound-var reference-id))])]
    [(stx _ _atom) (void)]))

;; stx? -> (or #f stx?)
(define (goto-definition reference)
  (define definition-id (hash-ref definitions (stx-node-id reference) #f))
  (and definition-id (hash-ref exprs definition-id #f)))

;; stx? -> (listof stx?)
(define (find-references id)
  (define definition (goto-definition id))
  (if definition
      (for/list ([reference-id (hash-ref references (stx-node-id definition) (list))])
        ;; assume expr exists
        (hash-ref exprs reference-id))
      (list)))

;; stx? -> void?
(define (analyze-types! syn)
 (infer-type syn)
 (void))

;; stx? (hasheq symbol? (or #f type?)) -> type?
;; #f in env means couldn't infer type
(define (infer-type syn [env (hasheq)])
  (define t
    (match syn
      [(stx-quote (let ([,(stx _ x) ,rhs]) ,body))
       (define t-rhs (infer-type rhs env))
       (infer-type body (hash-set env x t-rhs))]
      [(stx-quote (if ,cnd ,thn ,els))
       (infer-type cnd env)
       (define t-thn (infer-type thn env))
       (define t-els (infer-type els env))
       (cond
         [(and t-thn t-els (equal? t-thn t-els))
          t-thn]
         [else
          (record-static-error! (stx-node-id els) (type-mismatch (stx-node-id els) t-thn t-els))
          #f])]
      [(stx _ (? symbol? x))
       ;; assume unbound var is already checked for
       (hash-ref env x #f)]
      [(stx _ (? number?))
       'number]
      [(stx _ (? boolean?))
       'boolean]
      [_ #f]))
  (hash-set! types (stx-node-id syn) t)
  t)

;; -> (listof static-error?)
(define (get-static-errors)
  (for*/list ([(_ errs) (in-hash errors)]
              [err errs])
    err))