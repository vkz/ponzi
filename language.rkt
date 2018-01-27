#lang racket

(require (for-syntax syntax/parse
                     (only-in racket/pretty pretty-print))
         (only-in racket/pretty pretty-print)
         binaryio/integer)

(provide (rename-out [module-begin #%module-begin])
         #%datum
         push1
         add
         return)

(struct state (stack) #:mutable)

(define current-state (make-parameter (state '())))

;; (define-syntax (push1 stx)
;;   (syntax-parse stx
;;     [(_ (~alt (~once (~seq #:args (arg ...)))
;;               (~once (~seq #:addr addr:number))) ...)
;;      body]))

(define (push1 #:args args #:addr addr state)
  (set-state-stack! state (cons (first args) (state-stack state))))

(define (add #:args args #:addr addr state)
  (match (state-stack state)
    [(list a b stack ...) (set-state-stack! state (cons (+ a b) stack))]))

(define (return #:args args #:addr addr state)
  (pretty-print (state-stack state))
  (state-stack state))

(define-for-syntax (i/addr instructions ctx)
  (define addr #x00)
  (datum->syntax
   ctx
   (for/list ([i instructions])
     (syntax-parse i
       [(op:id arg ...) (begin0
                            #`(op #:args (list arg ...)
                                  #:addr #,(datum->syntax i addr)
                                  (current-state))
                          (set! addr (if (equal? (syntax->datum #'op) 'push1)
                                         (+ addr #x02)
                                         (+ addr #x01))))]))))

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(_  asm ...)
     (with-syntax ([(body ...) (i/addr (syntax-e #'(asm ...)) stx)])
       #`(#%plain-module-begin
          (parameterize ([current-state (state empty)])
            (pretty-print '(body ...))
            body ...)))]))

(define (syntax-srcloc stx)
  (srcloc (syntax-source   stx)
          (syntax-line     stx)
          (syntax-column   stx)
          (syntax-position stx)
          (syntax-span     stx)))
