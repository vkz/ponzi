#lang racket

(require racket/undefined
         (only-in binaryio
                  integer->bytes
                  bytes->integer))
(provide (all-defined-out))

;; σ in YP
(define *blockchain* (make-parameter (hasheq)))
(define (find-account addr)
  ((*blockchain*) addr))

(struct account (address balance))

(struct Halt (state) #:transparent)
(struct Stop Halt () #:transparent)
(struct Suicide Halt () #:transparent)
(struct Return Halt (bytes) #:transparent)
(struct Exn Halt (reason) #:transparent)

(define halt
  (match-lambda*
    [(list 'STOP)         (Stop (state-snapshot))]
    [(list 'SUICIDE)      (Suicide (state-snapshot))]
    [(list 'RETURN bytes) (Return (state-snapshot) bytes)]
    [(list 'EXN reason)   (Exn (state-snapshot) reason)]))

(struct mu (gas pc mem i stack) #:transparent)
(define (state-snapshot)
  (mu (*gas*)
      (*pc*)
      (mem)
      (bytes-length (mem))
      (stack)))

(struct I (account origin gasprice data sender value bytecode calldepth)
  #:transparent)

(struct exn:evm exn (state) #:transparent)
(struct exn:evm:out-of-gas exn:evm () #:transparent)
(struct exn:evm:stack-underflow exn:evm () #:transparent)
(struct exn:evm:stack-overflow exn:evm () #:transparent)
(struct exn:evm:jumpdest exn:evm () #:transparent)
(struct exn:evm:stop exn:evm () #:transparent)

(define (make-exn-evm exn-type msg)
  (λ () (exn-type (string-append msg
                                 (format " at instruction ~a"
                                         (instructions (*pc*))))
                  (current-continuation-marks)
                  (state-snapshot))))
(define make-exn:evm:out-of-gas (make-exn-evm exn:evm:out-of-gas "Out of gas"))
(define make-exn:evm:stack-underflow (make-exn-evm exn:evm:stack-underflow "Stack underflow"))
(define make-exn:evm:stack-overflow (make-exn-evm exn:evm:stack-overflow "Stack overflow"))
(define make-exn:evm:jumpdest (make-exn-evm exn:evm:jumpdest "Not a jumpdest"))
(define make-exn:evm:stop (make-exn-evm exn:evm:stop "Stopped"))

(define evm-raise (match-lambda*
                    [(list 'out-of-gas) (raise (make-exn:evm:out-of-gas))]
                    [(list 'underflow)  (raise (make-exn:evm:stack-underflow))]
                    [(list 'overflow)   (raise (make-exn:evm:stack-overflow))]
                    [(list 'jumpdest)   (raise (make-exn:evm:jumpdest))]
                    [(list 'stop)       (raise (make-exn:evm:stop))]))

;; TODO when populating *instructions* it is safe to add STOP as the very last instruction
;; which IIUC YP implies (see def of ω in section 9).
(define *instructions* (make-parameter (hasheq)))
(define *log*      (make-parameter '()))
(define *suicides* (make-parameter '()))
(define *gas*      (make-parameter 0))
(define *pc*       (make-parameter 0))
(define *store*    (make-parameter (hasheq)))
(define *stack*    (make-parameter '()))
(define *mem*      (make-parameter (bytes)))
(define *I*        (make-parameter (I undefined undefined 0 #"" undefined 0 #"" 0)))

(define (info-account)   (I-account (*I*)))
(define (info-origin)    (I-origin (*I*)))
(define (info-gasprice)  (I-gasprice (*I*)))
(define (info-data)      (I-data (*I*)))
(define (info-sender)    (I-sender (*I*)))
(define (info-value)     (I-value (*I*)))
(define (info-bytecode)  (I-bytecode (*I*)))
(define (info-calldepth) (I-calldepth (*I*)))

(define instructions
  (case-lambda
    [()   (*instructions*)]
    [(pc) (hash-ref (*instructions*) pc)]))

(define log (match-lambda*
              [(list) (*log*)]
              [(list offset size topic ...)
               ;; (assert (<= (length topic) 4))
               (*log* (list*
                       (cons topic (mem offset size))
                       (*log*)))]))

(define store (case-lambda
                [()    (*store*)]
                ;; YP doesn't specify what happens if no such key
                [(k)   (hash-ref (*store*) k)]
                ;; Must use gas, alter substate
                [(k v) (*store* (hash-set (*store*) k v))]))


(define (stack/overflow! s)
  (when (> (length s) 1024) (evm-raise 'overflow))
  (*stack* s))

(define stack (match-lambda*
                ;; get current stack value
                [(list)              (*stack*)]
                ;; reset stack to a new value
                [(list (list s ...)) (stack/overflow! s)]
                ;; push element on the stack
                [(list e)            (stack/overflow! (cons e (*stack*)))]
                ;; construct stack from s with e on top, use to replace top element after
                ;; pattern matching on stack
                [(list e s)          (stack/overflow! (cons e s))]))

(define mem (match-lambda*
              ;; get a copy of the entire memory
              [(list) (bytes-copy (*mem*))]
              ;; copy 32 bytes from memory at offset
              [(list offset) (mem offset 32)]
              ;; copy size bytes from memory at offset
              [(list offset size) (begin
                                    (malloc! (+ offset size))
                                    (subbytes (*mem*) offset (+ offset size)))]))

(define (malloc! offset)
  ;; TODO enforce size always multiple of 32
  (let* ((memory      (*mem*))
         (size        (bytes-length memory))
         (min-size    (let-values ([(q r) (quotient/remainder offset 32)])
                        (if (zero? r)
                            offset
                            (* 32 (add1 q)))))
         (delta       (max 0 (- min-size size)))
         (delta-bytes (make-bytes delta)))
    ;; TODO bookkeeping e.g. increasing index, checking gas
    (when (positive? delta)
      (*mem* (bytes-append memory delta-bytes)))))

(define (unimplemented op)
  (error op "operator ~a not implemented" op))

;; TODO Proper EVM word256, byte and bit manipulation. Options are:
;; 1 - bitvectors library
;; 2 - extend RnRS bytevectors
;; 3 - Rosette's bitvectors

;; NOTE Re instruction statefulness. Our instructions match YP semantics. They communicate
;; by changing state. This is ok, but then those effects are implicit in the
;; implementation of each instruction. Consequently it isn't easy to determine the effect
;; of one just by examining the signature. The answer could be to either rewrite each
;; instruction as pure function or somehow write contracts (pre/post) to each instruction.
;; Or maybe even encode invariants in types. Contracts would be nice cause they
;; communicate in code what each instruction requires as well as what effects each one
;; has. Contracts should probably be expressed in terms of EVM and system state.

;;** 0x0_

;; (#x00 STOP       0 0 1)
;; (#x01 ADD        2 1 1)
;; (#x02 MUL        2 1 1)
;; (#x03 SUB        2 1 1)
;; (#x04 DIV        2 1 1)
;; (#x05 SDIV       2 1 1)
;; (#x06 MOD        2 1 1)
;; (#x07 SMOD       2 1 1)
;; (#x08 ADDMOD     3 1 1)
;; (#x09 MULMOD     2 1 1)
;; (#x0a EXP        2 1 1)
;; (#x0b SIGNEXTEND 2 1 1)

(define (stop)
  (pc++)
  (halt 'STOP))

(define (add)
  (match (stack)
    [(list l r s ...) (stack (+ l r) s)]
    [(list _)         (evm-raise 'underflow)]
    [(list)           (evm-raise 'underflow)])
  (pc++))

(define (mul)
  (match (stack)
    [(list l r s ...) (stack (* l r) s)]
    [(list _)         (evm-raise 'underflow)]
    [(list)           (evm-raise 'underflow)])
  (pc++))

(define (sub)
  (match (stack)
    [(list l r s ...) (stack (- l r) s)]
    [(list _)         (evm-raise 'underflow)]
    [(list)           (evm-raise 'underflow)])
  (pc++))

(define (div)
  ;; Racket's / is signed! Different semantics?
  (match (stack)
    [(list l 0 s ...) (stack 0 s)]
    [(list l r s ...) (stack (/ l r) s)]
    [(list _)         (evm-raise 'underflow)]
    [(list)           (evm-raise 'underflow)])
  (pc++))

(define (sdiv)
  (unimplemented 'sdiv))

(define (mod)
  ;; Racket's modulo is signed! So, I suspect we actually implement /smod/ here
  (match (stack)
    [(list l 0 s ...)   (stack 0 s)]
    [(list l r s ...)   (stack (remainder l r) s)]
    [(list _ _)         (evm-raise 'underflow)]
    [(list _)           (evm-raise 'underflow)]
    [(list)             (evm-raise 'underflow)])
  (pc++))

(define (smod)
  (unimplemented 'smod))

(define (addmod)
  (match (stack)
    [(list l r 0 s ...) (stack 0 s)]
    [(list l r m s ...) (stack (remainder (+ l r)) s)]
    [(list _ _)         (evm-raise 'underflow)]
    [(list _)           (evm-raise 'underflow)]
    [(list)             (evm-raise 'underflow)])
  (pc++))

(define (mulmod)
  (match (stack)
    [(list l r 0 s ...) (stack 0 s)]
    [(list l r m s ...) (stack (remainder (* l r)) s)]
    [(list _ _)         (evm-raise 'underflow)]
    [(list _)           (evm-raise 'underflow)]
    [(list)             (evm-raise 'underflow)])
  (pc++))

(define (exp)
  (match (stack)
    [(list l r s ...) (stack (expt l r) s)]
    [(list _)         (evm-raise 'underflow)]
    [(list)           (evm-raise 'underflow)])
  (pc++))

(define (signextend)
  (unimplemented 'signextend))

;;** 0x1_

;; (#x10 LT         2 1 1)
;; (#x11 GT         2 1 1)
;; (#x12 SLT        2 1 1)
;; (#x13 LGT        2 1 1)
;; (#x14 EQ         2 1 1)
;; (#x15 ISZERO     1 1 1)
;; (#x16 AND        2 1 1)
;; (#x17 OR         2 1 1)
;; (#x18 XOR        2 1 1)
;; (#x19 NOT        1 1 1)
;; (#x1a BYTE       2 1 1)

(define (lt)
  ;; Racket semantics are signed!
  (match (stack)
    [(list l r s ...) (stack (if (< l r) 1 0) s)]
    [(list _)         (evm-raise 'underflow)]
    [(list)           (evm-raise 'underflow)])
  (pc++))

(define (gt)
  (match (stack)
    [(list l r s ...) (stack (if (> l r) 1 0) s)]
    [(list _)         (evm-raise 'underflow)]
    [(list)           (evm-raise 'underflow)])
  (pc++))

(define (slt)
  (unimplemented 'slt))

(define (lgt)
  (unimplemented 'sgt))

(define (eq)
  (match (stack)
    [(list l r s ...) (stack (if (equal? l r) 1 0) s)]
    [(list _)         (evm-raise 'underflow)]
    [(list)           (evm-raise 'underflow)])
  (pc++))

(define (iszero)
  (match (stack)
    [(list v s ...) (stack (if (zero? v) 1 0) s)]
    [(list)         (evm-raise 'underflow)])
  (pc++))

(define (and)
  (match (stack)
    [(list l r s ...) (stack (bitwise-and l r) s)]
    [(list _)         (evm-raise 'underflow)]
    [(list)           (evm-raise 'underflow)])
  (pc++))

(define (or)
  (match (stack)
    [(list l r s ...) (stack (bitwise-ior l r) s)]
    [(list _)         (evm-raise 'underflow)]
    [(list)           (evm-raise 'underflow)])
  (pc++))

(define (xor)
  (match (stack)
    [(list l r s ...) (stack (bitwise-xor l r) s)]
    [(list _)         (evm-raise 'underflow)]
    [(list)           (evm-raise 'underflow)])
  (pc++))

(define (not)
  (match (stack)
    [(list v s ...) (stack (bitwise-not v) s)]
    [(list)         (evm-raise 'underflow)])
  (pc++))

(define (byte)
  (match (stack)
    ;; EVM is big endian and assumes /at/ count starting from the left, so /at/=0 is the
    ;; most significant byte. Racket doesn't work in terms of left or right, but rather
    ;; /start/ position is closer to the least significant bit, /end/ closer to the most
    ;; significant. Imo on a little endian machine Racket's /bitwise-bit-field/
    ;; effectively like EVM expects, but we'd need to fiddle with offset on a big endian
    ;; machine e.g. offset = (- 32 (add1 offset)).
    [(list at n s ...) (stack (bitwise-bit-field n at 8) s)]
    [(list _)          (evm-raise 'underflow)]
    [(list)            (evm-raise 'underflow)])
  (pc++))

;;** 0x2_

;; (#x20 SHA3 2 1 1)

(define (sha3)
  (unimplemented 'sha3))

;;** 0x3_

;; (#x30 ADDRESS      0 1 1)
;; (#x31 BALANCE      1 1 1)
;; (#x32 ORIGIN       0 1 1)
;; (#x33 CALLER       0 1 1)
;; (#x34 CALLVALUE    0 1 1)
;; (#x35 CALLDATALOAD 1 1 1)
;; (#x36 CALLDATASIZE 0 1 1)
;; (#x37 CALLDATACOPY 3 1 1)
;; (#x38 CODESIZE     0 1 1)
;; (#x39 CODECOPY     3 0 1)
;; (#x3a GASPRICE     0 1 1)
;; (#x3b EXTCODESIZE  1 1 1)
;; (#x3c EXTCODECOPY  4 0 1)

(define (address)
  (stack (info-account))
  (pc++))

(define (balance)
  (match (stack)
    [(list (app find-account a) s ...) (stack (or (account-balance a) 0) s)]
    [(list)                            (evm-raise 'underflow)])
  (pc++))

(define (origin)
  (stack (info-origin))
  (pc++))

(define (caller)
  (stack (info-sender))
  (pc++))

(define (callvalue)
  (stack (info-value))
  (pc++))

(define (calldataload)
  (match (stack)
    [(list offset s ...) (stack (subbytes/0 (info-data) offset (+ offset 32)))]
    [(list)              (evm-raise 'underflow)])
  (pc++))

(define (calldatasize)
  (stack (bytes-length (info-data)))
  (pc++))

(define (get-subbytes bytes start end #:stop stop-thunk)
  (let* ((size   (- end start))
         (bs     (make-bytes size 0))
         (start  start)
         (stop   (min (bytes-length bytes) (+ start size)))
         (copied (- stop start))
         (filled (max 0 (- size copied))))
    (when (and stop-thunk (positive? filled))
      (stop-thunk))
    (bytes-copy! bs 0 bytes start stop)
    bs))

;; TODO How to STOP EVM mid execution?
(define (subbytes/stop bytes start end)
  (get-subbytes bytes start end #:stop (λ () (evm-raise 'stop))))

(define (subbytes/0 bytes start end)
  (get-subbytes bytes start end #:stop #f))

(define mwrite!
  (match-lambda*
    [(list mem-offset from-bytes)
     (mwrite! mem-offset from-bytes 0 (bytes-length from-bytes))]
    [(list mem-offset from-bytes from-offset size)
     (malloc! (+ mem-offset size))
     (bytes-copy! (*mem*)
                  mem-offset
                  from-bytes
                  from-offset
                  (+ from-offset size))]))

(define (calldatacopy)
  (match (stack)
    [(list mem-offset data-offset size s ...)
     (mwrite! mem-offset
              (subbytes/0 (info-data)
                          data-offset
                          (+ data-offset size)))
     (stack s)]
    [(list _ _) (evm-raise 'underflow)]
    [(list _)   (evm-raise 'underflow)]
    [(list)     (evm-raise 'underflow)])
  (pc++))

(define (codesize)
  (stack (bytes-length (info-bytecode)))
  (pc++))

(define (codecopy)
  (match (stack)
    [(list mem-offset code-offset size s ...)
     (mwrite! mem-offset
              (subbytes/stop (info-bytecode)
                             code-offset
                             (+ code-offset size)))
     (stack s)]
    [(list _ _) (evm-raise 'underflow)]
    [(list _)   (evm-raise 'underflow)]
    [(list)     (evm-raise 'underflow)])
  (pc++))

(define (gasprice)
  (stack (info-gasprice))
  (pc++))

(define (extcodesize)
  (match (stack)
    [(list (app account a) s ...) (stack (bytes-length (info-bytecode a)) s)]
    [(list)                       (evm-raise 'underflow)])
  (pc++))

(define (extcodecopy)
  (match (stack)
    [(list (app account a) mem-offset code-offset size s ...)
     (mwrite! mem-offset
              (subbytes/stop (info-bytecode a)
                             code-offset
                             (+ code-offset size)))
     (stack s)]
    [(list _ _) (evm-raise 'underflow)]
    [(list _)   (evm-raise 'underflow)]
    [(list)     (evm-raise 'underflow)])
  (pc++))

;;** x4_
;;
;; Chain specific - might be useful for some contracts, but we can do without for now.
;;
;; (#x40 BLOCKHASH  1 1 1)
;; (#x41 COINBASE   0 1 1)
;; (#x42 TIMESTAMP  0 1 1)
;; (#x43 NUMBER     0 1 1)
;; (#x44 DIFFICULTY 0 1 1)
;; (#x45 GASLIMIT   0 1 1)

(define (blockhash) (unimplemented 'blockhash))
(define (coinbase) (unimplemented 'coinbase))
(define (timestamp) (unimplemented 'timestamp))
(define (number) (unimplemented 'number))
(define (difficulty) (unimplemented 'difficulty))
(define (gaslimit) (unimplemented 'gaslimit))

;;** x5_
;; (#x50 POP          1 0 1)
;; (#x51 MLOAD        1 1 1)
;; (#x52 MSTORE       2 0 1)
;; (#x53 MSTORE8      2 0 1)
;; (#x54 SLOAD        1 1 1)
;; (#x55 SSTORE       2 0 1)
;; (#x56 JUMP         1 0 1)
;; (#x57 JUMPI        2 0 1)
;; (#x58 PC           0 1 1)
;; (#x59 MSIZE        0 1 1)
;; (#x5a GAS          0 1 1)
;; (#x5b JUMPDEST     0 0 1)

(define (pop)
  (match (stack)
    [(list _ s ...) (stack s)]
    [(list)         (evm-raise 'underflow)])
  (pc++))

(define (mload)
  (match (stack)
    [(list offset s ...) (stack (mem offset) s)]
    [(list)              (evm-raise 'underflow)])
  (pc++))

(define (mstore)
  (match (stack)
    [(list offset n s ...) (mwrite! offset
                                    (integer->bytes n 32 #f))
                           (stack s)]
    [(list _)              (evm-raise 'underflow)]
    [(list)                (evm-raise 'underflow)])
  (pc++))

(define (mstore8)
  (match (stack)
    [(list offset n s ...) (mwrite! offset
                                    ;; truncate n to the lowest byte before converting to
                                    ;; bytes and writing
                                    (integer->bytes
                                     (bitwise-and n #xff) 1))
                           (stack s)]
    [(list _)              (evm-raise 'underflow)]
    [(list)                (evm-raise 'underflow)])
  (pc++))

(define (sload)
  (match (stack)
    [(list k s ...) (stack (store k) s)]
    [(list)         (evm-raise 'underflow)])
  (pc++))

(define (sstore)
  (match (stack)
    [(list k v s ...) (store k v)
                      (stack s)]
    [(list _)         (evm-raise 'underflow)]
    [(list)           (evm-raise 'underflow)])
  (pc++))

;; Every other instruction implies pc++. Should this be done explicitly?
(define (jump)
  (match (stack)
    [(list offset s ...) (stack s)
                         (pc offset)]
    [(list)              (evm-raise 'undeflow)]))

(define (jumpi)
  (match (stack)
    [(list offset test s ...) (stack s)
                              (if (zero? test)
                                (pc offset)
                                (pc++))]
    [(list _)                 (evm-raise 'underflow)]
    [(list)                   (evm-raise 'underflow)]))

(define (pc)
  (stack (*pc*))
  (pc++))

(define pc++ (case-lambda
               [() (*pc* (add1 (*pc*)))]
               [(n) (*pc* (+ n (*pc*)))]))

(define (msize)
  (stack (bytes-length (mem))))

(define (gas)
  (stack (*gas*))
  (pc++))

(define (jumpdest)
  (pc++))

;;** x6_ and x7_
;; (#x60 PUSH1        0 1 1)
;; (#x61 PUSH2        0 1 1)
;; (#x62 PUSH3        0 1 1)
;; (#x63 PUSH4        0 1 1)
;; (#x64 PUSH5        0 1 1)
;; (#x65 PUSH6        0 1 1)
;; (#x66 PUSH7        0 1 1)
;; (#x67 PUSH8        0 1 1)
;; (#x68 PUSH9        0 1 1)
;; (#x69 PUSH10       0 1 1)
;; (#x6a PUSH11       0 1 1)
;; (#x6b PUSH12       0 1 1)
;; (#x6c PUSH13       0 1 1)
;; (#x6d PUSH14       0 1 1)
;; (#x6e PUSH15       0 1 1)
;; (#x6f PUSH16       0 1 1)
;; (#x70 PUSH17       0 1 1)
;; (#x71 PUSH18       0 1 1)
;; (#x72 PUSH19       0 1 1)
;; (#x73 PUSH20       0 1 1)
;; (#x74 PUSH21       0 1 1)
;; (#x75 PUSH22       0 1 1)
;; (#x76 PUSH23       0 1 1)
;; (#x77 PUSH24       0 1 1)
;; (#x78 PUSH25       0 1 1)
;; (#x79 PUSH26       0 1 1)
;; (#x7a PUSH27       0 1 1)
;; (#x7b PUSH28       0 1 1)
;; (#x7c PUSH29       0 1 1)
;; (#x7d PUSH30       0 1 1)
;; (#x7e PUSH31       0 1 1)
;; (#x7f PUSH32       0 1 1)

(define ((push-n n) val)
  (stack
   (bytes->integer
    (integer->bytes val n #f)
    #f))
  (pc++)
  (pc++ n))

(match-define
  (list push1  push2  push3  push4  push5  push6  push7  push8
        push9  push10 push11 push12 push13 push14 push15 push16
        push17 push18 push19 push20 push21 push22 push23 push24
        push25 push26 push27 push28 push29 push30 push31 push32)
  (for/list ([n (in-range 1 33)])
    (push-n n)))

;;** x8_
;; (#x80 DUP1         1 2   1)
;; (#x81 DUP2         2 3   1)
;; (#x82 DUP3         3 4   1)
;; (#x83 DUP4         4 5   1)
;; (#x84 DUP5         5 6   1)
;; (#x85 DUP6         6 7   1)
;; (#x86 DUP7         7 8   1)
;; (#x87 DUP8         8 9   1)
;; (#x88 DUP9         9 10  1)
;; (#x89 DUP10        10 11 1)
;; (#x8a DUP11        11 12 1)
;; (#x8b DUP12        12 13 1)
;; (#x8c DUP13        13 14 1)
;; (#x8d DUP14        14 15 1)
;; (#x8e DUP15        15 16 1)
;; (#x8f DUP16        16 17 1)

(define (dup-nth n)
  (λ () (if (< (length (stack)) n)
            (evm-raise 'underflow)
            (begin (stack (list-ref (stack) (sub1 n)))
                   (pc++)))))

(match-define
  (list dup1  dup2  dup3  dup4  dup5  dup6  dup7  dup8
        dup9  dup10 dup11 dup12 dup13 dup14 dup15 dup16)
  (for/list ([n (in-range 1 17)])
    (dup-nth n)))

;;** x9_
;; (#x90 SWAP1        2 2   1)
;; (#x91 SWAP2        3 3   1)
;; (#x92 SWAP3        4 4   1)
;; (#x93 SWAP4        5 5   1)
;; (#x94 SWAP5        6 6   1)
;; (#x95 SWAP6        7 7   1)
;; (#x96 SWAP7        8 8   1)
;; (#x97 SWAP8        9 9   1)
;; (#x98 SWAP9        10 10 1)
;; (#x99 SWAP10       11 11 1)
;; (#x9a SWAP11       12 12 1)
;; (#x9b SWAP12       13 13 1)
;; (#x9c SWAP13       14 14 1)
;; (#x9d SWAP14       15 15 1)
;; (#x9e SWAP15       16 16 1)
;; (#x9f SWAP16       17 17 1)

(define (swap-nth n)
  (λ ()
    (when (< (length (stack)) (add1 n))
      (evm-raise 'underflow))
    (define 1st (first (stack)))
    (define nth (list-ref (stack) n))
    (stack nth (rest (list-set (stack) n 1st)))
    (pc++)))

(match-define
  (list swap1  swap2  swap3  swap4  swap5  swap6  swap7  swap8
        swap9  swap10 swap11 swap12 swap13 swap14 swap15 swap16)
  (for/list ([n (in-range 1 17)])
    (swap-nth n)))

;;** xA_
;; (#xa0 LOG0 2 0 1)
;; (#xa1 LOG1 3 0 1)
;; (#xa2 LOG2 4 0 1)
;; (#xa3 LOG3 5 0 1)
;; (#xa4 LOG3 6 0 1)

(define (log0)
  (match (stack)
    [(list offset size s ...) (log offset size)
                              (stack s)]
    [(list args ...)          #:when (λ () (< (length args) 2))
                              (evm-raise 'underflow)])
  (pc++))

(define (log1)
  (match (stack)
    [(list offset size t s ...) (log offset size t)
                                (stack s)]
    [(list args ...)            #:when (λ () (< (length args) 3))
                                (evm-raise 'underflow)])
  (pc++))

(define (log2)
  (match (stack)
    [(list offset size t1 t2 s ...) (log offset size t1 t2)
                                    (stack s)]
    [(list args ...)                #:when (λ () (< (length args) 4))
                                    (evm-raise 'underflow)])
  (pc++))

(define (log3)
  (match (stack)
    [(list offset size t1 t2 t3 s ...) (log offset size t1 t2 t3)
                                       (stack s)]
    [(list args ...)                   #:when (λ () (< (length args) 5))
                                       (evm-raise 'underflow)])
  (pc++))

(define (log4)
  (match (stack)
    [(list offset size t1 t2 t3 t4 s ...) (log offset size t1 t2 t3 t4)
                                          (stack s)]
    [(list args ...)                      #:when (λ () (< (length args) 6))
                                          (evm-raise 'underflow)])
  (pc++))

;;** xF_
;; (#xf0 CREATE       3 1 1)
;; (#xf1 CALL         7 1 1)
;; (#xf2 CALLCODE     7 1 1)
;; (#xf3 RETURN       2 0 1)
;; (#xf4 DELEGATECALL 6 1 1)
;; (#xfd REVERT       0 0 1)
;; (#xfe INVALID      0 0 1)
;; (#xff SUICIDE      1 0 1)

;; Programmaticaly CREATE account with associated code. Not the same as deploying a new
;; contract. This is like contract deploying a new contract - a meta-contract?
(define (create)

  (define (create-account #:value balance
                          #:code bytes)
    (unimplemented 'create-account))

  (match (stack)
    ;; TODO set all info parameters correctly.
    [(list value offset size s ...) (stack (create-account #:value value
                                                           #:code (mem offset size))
                                           s)
                                    ;; push the return of create-account on the stack:
                                    ;; 0 - if #:code execution failed
                                    ;; a - address of the created account
                                    ]
    [(list _ _) (evm-raise 'underflow)]
    [(list _)   (evm-raise 'underflow)]
    [(list)     (evm-raise 'underflow)])
  (pc++))

(define (message-call #:sender       s
                      #:originator   o
                      #:recipient    r          ; to? aka #:to
                      #:account/code c          ; usually c = r
                      #:gas          g
                      #:gasprice     p
                      #:value        v
                      #:value~       v~
                      #:input        i          ; d in YP - input bytearray
                      #:calldepth    e)
  (unimplemented 'mellage-call))

(define (callgas gas)
  (unimplemented 'callgas))

(define (call)
  (match (stack)
    ;; TODO set all info parameters correctly.
    [(list gas  to   value in-offset in-size out-offset out-size s ...)
     ;;    μ[0] μ[1] μ[2]  μ[3]      μ[4]    μ[5]       μ[6]
     (define-values (return-code return-bytes)
       (message-call #:sender       (info-account)
                     #:originator   (info-origin)
                     #:recipient    to
                     #:account/code to
                     #:gas          (callgas gas)
                     #:gasprice     (info-gasprice)
                     #:value        value
                     #:value~       value
                     #:input        (mem in-offset in-size)
                     #:calldepth    (add1 (info-calldepth))))
     ;; YP semantics, but imho if u get more bytes than expected u shouldn't just cut them
     ;; but rather signal an error.
     (if (> (bytes-length return-bytes) out-size)
         ;; returned more bytes than allowed
         (mwrite! out-offset return-bytes 0 out-size)
         ;; returned bytes
         (mwrite! out-offset return-bytes))
     (stack return-code s)]
    [(list args ...) #:when (λ () (< (length args) 7))
                     (evm-raise 'underflow)])
  (pc++))

(define (callcode)
  (match (stack)
    [(list gas to value in-offset in-size out-offset out-size s ...)
     (define-values (return-code return-bytes)
       (message-call #:sender       (info-account)
                     #:originator   (info-origin)
                     #:recipient    (info-account) ; diff from /call/
                     #:account/code to
                     #:gas          (callgas gas)
                     #:gasprice     (info-gasprice)
                     #:value        value
                     #:value~       value
                     #:input        (mem in-offset in-size)
                     #:calldepth    (add1 (info-calldepth))))
     (if (> (bytes-length return-bytes) out-size)
         (mwrite! out-offset return-bytes 0 out-size)
         (mwrite! out-offset return-bytes))
     (stack return-code s)]
    [(list args ...) #:when (λ () (< (length args) 7))
                     (evm-raise 'underflow)])
  (pc++))

(define (return)
  (match (stack)
    [(list offset size s ...) (stack s)
                              (pc++)
                              (halt 'RETURN (mem offset size))]
    [(list _) (evm-raise 'underflow)]
    [(list)   (evm-raise 'underflow)]))

(define (delegatecall)
  (match (stack)
    ;; 6 parameters on stack: /value/ parameter dropped
    [(list gas to in-offset in-size out-offset out-size s ...)
     (define-values (return-code return-bytes)
       (message-call #:sender       (info-sender)  ; diff from /codecall/
                     #:originator   (info-origin)
                     #:recipient    (info-account) ; diff from /call/
                     #:account/code to
                     #:gas          gas            ; diff from /call/
                     #:gasprice     (info-gasprice)
                     #:value        0              ; diff from /call/
                     #:value~       (info-value)   ; diff from /call/
                     #:input        (mem in-offset in-size)
                     #:calldepth    (add1 (info-calldepth))))
     (if (> (bytes-length return-bytes) out-size)
         (mwrite! out-offset return-bytes 0 out-size)
         (mwrite! out-offset return-bytes))
     (stack return-code s)]
    [(list args ...) #:when (λ () (< (length args) 6))
                     (evm-raise 'underflow)])
  (pc++))

(define (revert)
  (unimplemented 'revert))

(define (invalid)
  (unimplemented 'invalid))

(define (suicide)

  (define (send-money #:value balance
                      #:from  from-account
                      #:to    to-account)
    (unimplemented 'send-money))

  (define (refund)
    (unimplemented 'refund))

  (match (stack)
    [(list recipient s ...) (send-money (account-balance (info-account))
                                        #:from (info-account)
                                        #:to recipient)
                            ;; assert:
                            ;; recipient account balance += this account balance
                            ;; this account balance       = 0
                            (stack s)
                            (*suicides* (cons (info-account) (*suicides*)))
                            (refund)
                            (pc++)
                            (halt 'SUICIDE)]))

(module+ test
  (parameterize ((*pc*    0)
                 (*stack* '())
                 (*mem*   (bytes))
                 (*I*     (I undefined undefined 0 #"" undefined 0 #"" 0)))
    (push1 #x01)
    (push1 #x02)
    (add)
    (push1 1)
    (add)
    ;; mstore to return
    (push1 0)
    (mstore)
    (push1 32)
    (push1 0)
    (return)))
