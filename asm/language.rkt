#lang racket

(require (for-syntax
          racket/match
          syntax/parse
          (only-in racket/pretty pretty-print)
          (only-in "../assembler.rkt" instructions/patched-labels))
         (only-in racket/pretty pretty-print)
         "semantics.rkt"
         "evm.rkt")

(define-for-syntax (table-of-instructions is)
  #`(make-hash
     (list
      #,@(map
          (match-lambda [(list offset _ i) #`(list* #,offset (λ () #,i))])
          is))))

(module+ test
  (displayln "Test 1 instructions")
  (begin-for-syntax
    (pretty-print
     (table-of-instructions '((0 "6001" (push1 1))
                              (2 "6002" (push1 2))
                              (4 "01" (add))
                              (5 "61000e" (push 14))
                              (8 "56" (jump))
                              (9 "63ffffffff" (push4 4294967295))
                              (14 "5b" (label a))
                              (15 "6001" (push1 1))
                              (17 "01" (add))
                              (18 "f3" (return)))))))

(define-syntax (asm stx)
  (syntax-parse stx
    [(_ is ...) (let ((table (table-of-instructions
                              (instructions/patched-labels
                               (syntax->datum #'(is ...))))))
                  ;; lose hygiene
                  (with-syntax ([name (datum->syntax stx 'instructions)])
                    #`(parameterize ((*instructions* #,table))
                        (evm))))]))

(module+ test
  (displayln "Test 2 asm")
  (parameterize ((*pc*    0)
                 (*stack* '())
                 (*mem*   (bytes)))
    (asm (push1 #x01)
         (push1 #x02)
         (add)
         (push1 1)
         (add)
         ;; mstore to return
         (push1 0)
         (mstore)
         (push1 32)
         (push1 0)
         (return))))
