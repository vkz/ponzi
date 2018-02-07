#lang s-exp evm/asm-sim

;;* Context

;;** chain to use
(eth-state (chain "rinkeby"
                  #:asof (or (transaction #xff)
                             (block #xbb))))

;;** additional accounts
(define-account alice
  #:balance 10)
(define-account bob
  #:balance 0)

;;** additional contracts
(define-contract C
  ;; optional
  #:address #x01
  #:balance 100
  ;; (define slot-a (kechack256 "slot-a"))
  #:store ((slot-a . #x01)
           (slot-b . #x02))
  #:abi ((get-a #:code-offset #x00
                #:args ((arg1 . d-offset1)
                        ;; (arg2 . d-offset2)
                        )))
  #:code (asm
          ;; get slot from input
          (push 0)                     ;at    s[0]
          (calldataload)               ;      s[slot]
          ;; get val from store
          (sload)
          ;; write val to mem
          (push 0)                     ;at    s[0 #x01]
          (mstore)                     ;      s[]       m[00: 01]
          ;; memory to return
          (push 1)                     ;1byte s[1]      m[00: 01]
          (push 0)                     ;at    s[0 1]    m[00: 01]
          ;; return
          (return)))

;;* Contract

;;** evm state
(stack [])
(store (make-hash))
(data #"")
(balance 300000)

;;** constants
(define gas 10000)
(define to (contract C))
(define value 0)
(define in-offset 0)
(define in-size 32)
(define out-offset (+ in-offset in-size))
(define out-size 1)

(define slot-a (kechack256 "slot-a"))

;;** code to execute
(asm
 ;; mstore input data for call to C
 (push slot-a)
 (push in-offset)
 (mstore)
 ;; put call args on stack
 (push out-size)
 (push out-offset)
 (push in-size)
 (push in-offset)
 (push value)
 (push to)
 (push gas)
 (call)
 ;; add 2 to the returned slot value
 (push out-offset)
 (mload)
 (push #x02)
 (add)
 ;; skip instruction
 (push (label a))
 (jump)
 (push4 #xffffffff)
 (label a)
 ;; increment by 1 again
 (push 1)
 (add)
 ;; mstore to return
 (push 0)                               ; at       s[0 4]
 (mstore)                               ;          s[]     m[00: 4]
 (push 32)                              ; 32 bytes s[32]   m[00: 4]
 (push 0)                               ; at       s[0 32] m[00: 4]
 (return))
