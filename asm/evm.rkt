#lang racket

(require "semantics.rkt")

(define (on-exn e) (halt 'EXN e))
;; (define (handle-stop e) (halt 'STOP))

(define (step!)
  (with-handlers ([exn:evm:out-of-gas?      on-exn]
                  [exn:evm:stack-underflow? on-exn]
                  [exn:evm:stack-overflow?  on-exn]
                  [exn:evm:jumpdest?        on-exn]
                  [exn:evm:stop?            on-exn]
                  ;; [exn:fail?                (λ (e)
                  ;;                             (log-error
                  ;;                              "exn default handler"))]
                  )
    (let ((instr (hash-ref instructions (pc))))
      (instr))))

(module+ test

  (asm
   (push1 #x01)
   (push1 #x02)
   (add)
   (push (label a))
   (jump)
   (push4 #xffffffff)
   (label a)
   (push1 1)
   (add)
   (return))

  ;; =>

  (*instructions*
   (make-hasheq
    (list (0  (λ () (push1 1)))
          (2  (λ () (push1 2)))
          (4  (λ () (add)))
          (5  (λ () (push 14)))
          (8  (λ () (jump)))
          (9  (λ () (push4 4294967295)))
          (14 (λ () (label a)))
          (15 (λ () (push1 1)))
          (17 (λ () (add)))
          (18 (λ () (return))))))

  ;; end test
  )
