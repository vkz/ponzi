#lang racket

(require "semantics.rkt")
(provide evm)

(define (on-exn e) (halt 'EXN e))
;; (define (handle-stop e) (halt 'STOP))

(define (evm)
  ;; TODO we use the fact that some EVM instructions like (return) actually return values.
  ;; This is contrary to every other instruction that only alter machine state. Be
  ;; consistent and have all instructions just alter machine state, have EVM detect
  ;; halting state.
  (match (step!)
    [(? Halt? h) h]
    [_ (evm)]))

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
    (let ((instr (hash-ref (*instructions*) (*pc*))))
      (instr))))

(module+ test
  (parameterize ((*instructions*
                  (make-hasheq
                   (list
                    (list* 0  (λ () (push1 #x01)))
                    (list* 2  (λ () (push1 #x02)))
                    (list* 4  (λ () (add)))
                    (list* 5  (λ () (push1 1)))
                    (list* 7  (λ () (add)))
                    (list* 8  (λ () (push1 0)))
                    (list* 10 (λ () (mstore)))
                    (list* 11 (λ () (push1 32)))
                    (list* 13 (λ () (push1 0)))
                    (list* 15 (λ () (return)))))))
    (evm))

  ;; end test
  )
