#lang racket
(require binaryio/integer
         file/sha1
         (for-syntax syntax/parse
                     (only-in racket/pretty pretty-print)))

(provide instructions/patched-labels
         assemble
         disassemble)

;; loc len (op arg)                hex-string
;; -------------------------------------------------------
;; 00  2   (push1 #x01)            ; 60__          => 6001
;; 02  2   (push1 #x02)            ; 60__          => 6002
;; 04  1   (add)                   ; 01            => 01
;; 05  2   (push1 (label a))       ; 60__          => 600D
;; 07  1   (jump)                  ; 56            => 56
;; 08  5   (push4 #xffffffff)      ; 63__|__|__|__ => 63FFFFFFFF
;; 0D  1   (label a)               ; 5b            => 5B
;; 0E  2   (push1 1)               ; 60__          => 6001
;; 10  1   (add)                   ; 01            => 01
;; 11  1   (return)                ; f3            => F3
;;
;; push1 push1 add push1 jump push4      jumpdest push1 add return
;; 6001  6002  01  600D  56   63FFFFFFFF 5B       6001  01  F3

;; TODO better assert
(define-syntax (assert stx)
  (syntax-parse stx
    [(_ test)
     #`(unless test
         (raise-user-error
          'assert
          (format "failed in~n~.a" 'test)))]))

(struct is (bytecode opcode pops pushes size bytes) #:transparent)

(define isa
  (map (match-lambda
         [(list bytecode opcode pops pushes size)
          ;; (#x60 PUSH1 0 1 1)
          ;; ---------------------
          ;; (is 96 'PUSH1 0 1 1 #"`")
          (is bytecode opcode pops pushes size (integer->bytes bytecode size #f))])
       '(
         ;; x0_
         (#x00 STMAKE-O   0 0 1)
         (#x01 ADD        2 1 1)
         (#x02 MUL        2 1 1)
         (#x03 SUB        2 1 1)
         (#x04 DIV        2 1 1)
         (#x05 SDIV       2 1 1)
         (#x06 MOD        2 1 1)
         (#x07 SMOD       2 1 1)
         (#x08 ADDMOD     3 1 1)
         (#x09 MULMOD     2 1 1)
         (#x0a EXP        2 1 1)
         (#x0b SIGNEXTEND 2 1 1)

         ;; x1_
         (#x10 LT         2 1 1)
         (#x11 GT         2 1 1)
         (#x12 SLT        2 1 1)
         (#x13 LGT        2 1 1)
         (#x14 EQ         2 1 1)
         (#x15 ISZERO     1 1 1)
         (#x16 AND        2 1 1)
         (#x17 OR         2 1 1)
         (#x18 XOR        2 1 1)
         (#x19 NOT        1 1 1)
         (#x1a BYTE       2 1 1)

         ;; x2_
         (#x20 SHA3       2 1 1)

         ;; x3_
         (#x30 ADDRESS      0 1 1)
         (#x31 BALANCE      1 1 1)
         (#x32 ORIGIN       0 1 1)
         (#x33 CALLER       0 1 1)
         (#x34 CALLVALUE    0 1 1)
         (#x35 CALLDATALOAD 1 1 1)
         (#x36 CALLDATASIZE 0 1 1)
         (#x37 CALLDATACOPY 3 1 1)
         (#x38 CODESIZE     0 1 1)
         (#x39 CODECOPY     3 0 1)
         (#x3a GASPRICE     0 1 1)
         (#x3b EXTCODESIZE  1 1 1)
         (#x3c EXTCODECOPY  4 0 1)

         ;; x4_
         (#x40 BLOCKHASH    1 1 1)
         (#x41 COINBASE     0 1 1)
         (#x42 TIMESTAMP    0 1 1)
         (#x43 NUMBER       0 1 1)
         (#x44 DIFFICULTY   0 1 1)
         (#x45 GASLIMIT     0 1 1)

         ;; x5_
         (#x50 P            1 0 1)
         (#x51 MLOAD        1 1 1)
         (#x52 MSTORE       2 0 1)
         (#x53 MSTORE8      2 0 1)
         (#x54 SLOAD        1 1 1)
         (#x55 SSTORE       2 0 1)
         (#x56 JUMP         1 0 1)
         (#x57 JUMPI        2 0 1)
         (#x58 PC           0 1 1)
         (#x59 MSIZE        0 1 1)
         (#x5a GAS          0 1 1)
         (#x5b JUMPDEST     0 0 1)

         ;; x6_ and x7_
         (#x60 PUSH1        0 1 1)
         (#x61 PUSH2        0 1 1)
         (#x62 PUSH3        0 1 1)
         (#x63 PUSH4        0 1 1)
         (#x64 PUSH5        0 1 1)
         (#x65 PUSH6        0 1 1)
         (#x66 PUSH7        0 1 1)
         (#x67 PUSH8        0 1 1)
         (#x68 PUSH9        0 1 1)
         (#x69 PUSH10       0 1 1)
         (#x6a PUSH11       0 1 1)
         (#x6b PUSH12       0 1 1)
         (#x6c PUSH13       0 1 1)
         (#x6d PUSH14       0 1 1)
         (#x6e PUSH15       0 1 1)
         (#x6f PUSH16       0 1 1)
         (#x70 PUSH17       0 1 1)
         (#x71 PUSH18       0 1 1)
         (#x72 PUSH19       0 1 1)
         (#x73 PUSH20       0 1 1)
         (#x74 PUSH21       0 1 1)
         (#x75 PUSH22       0 1 1)
         (#x76 PUSH23       0 1 1)
         (#x77 PUSH24       0 1 1)
         (#x78 PUSH25       0 1 1)
         (#x79 PUSH26       0 1 1)
         (#x7a PUSH27       0 1 1)
         (#x7b PUSH28       0 1 1)
         (#x7c PUSH29       0 1 1)
         (#x7d PUSH30       0 1 1)
         (#x7e PUSH31       0 1 1)
         (#x7f PUSH32       0 1 1)

         ;; x8_
         (#x80 DUP1         1 2   1)
         (#x81 DUP2         2 3   1)
         (#x82 DUP3         3 4   1)
         (#x83 DUP4         4 5   1)
         (#x84 DUP5         5 6   1)
         (#x85 DUP6         6 7   1)
         (#x86 DUP7         7 8   1)
         (#x87 DUP8         8 9   1)
         (#x88 DUP9         9 10  1)
         (#x89 DUP10        10 11 1)
         (#x8a DUP11        11 12 1)
         (#x8b DUP12        12 13 1)
         (#x8c DUP13        13 14 1)
         (#x8d DUP14        14 15 1)
         (#x8e DUP15        15 16 1)
         (#x8f DUP16        16 17 1)

         ;; x9_
         (#x90 SWAP1        2 2   1)
         (#x91 SWAP2        3 3   1)
         (#x92 SWAP3        4 4   1)
         (#x93 SWAP4        5 5   1)
         (#x94 SWAP5        6 6   1)
         (#x95 SWAP6        7 7   1)
         (#x96 SWAP7        8 8   1)
         (#x97 SWAP8        9 9   1)
         (#x98 SWAP9        10 10 1)
         (#x99 SWAP10       11 11 1)
         (#x9a SWAP11       12 12 1)
         (#x9b SWAP12       13 13 1)
         (#x9c SWAP13       14 14 1)
         (#x9d SWAP14       15 15 1)
         (#x9e SWAP15       16 16 1)
         (#x9f SWAP16       17 17 1)

         ;; xA_
         (#xa0 LOG0 2 0 1)
         (#xa1 LOG1 3 0 1)
         (#xa2 LOG2 4 0 1)
         (#xa3 LOG3 5 0 1)
         (#xa4 LOG3 6 0 1)

         ;; xF_
         (#xf0 CREATE       3 1 1)
         (#xf1 CALL         7 1 1)
         (#xf2 CALLCODE     7 1 1)
         (#xf3 RETURN       2 0 1)
         (#xf4 DELEGATECALL 6 1 1)
         (#xfd REVERT       0 0 1)
         (#xfe INVALID      0 0 1)
         (#xff SUICIDE      1 0 1))))

(define isa-by-opcode
  (make-immutable-hash
   (map (λ (is) (cons (is-opcode is) is)) isa)))

(define isa-by-bytecode
  (make-immutable-hash
   (map (λ (is) (cons (is-bytecode is) is)) isa)))

(define isa-by-bytes
  (make-immutable-hash
   (map (λ (is) (cons (is-bytes is) is)) isa)))

(define (bytes-of-opcode opcode)
  (is-bytes (hash-ref isa-by-opcode opcode)))

(define (push? opcode)
  (let ((bytecode (is-bytecode
                   (hash-ref isa-by-opcode opcode))))
    (<= #x60 bytecode #x7f)))

(define (push-arg-size opcode)
  (assert (push? opcode))
  (let* ((push-n (hash-ref isa-by-opcode opcode))
         (push-1 (hash-ref isa-by-opcode 'PUSH1)))
    (add1 (- (is-bytecode push-n)
             (is-bytecode push-1)))))

(define (instruction-size opcode)
  (let ((is (hash-ref isa-by-opcode opcode)))
    (if (push? opcode)
        (let* ((push-1 (hash-ref isa-by-opcode 'PUSH1))
               (push-is-size (is-size is))
               (push-arg-size (add1 (- (is-bytecode is)
                                       (is-bytecode push-1)))))
          (+ push-is-size push-arg-size))
        (is-size is))))

(define (instructions/patched-labels instructions)
  (define labels (make-hash))
  (define offset 0)
  (define (offset+ size)
    (begin0 offset
      (set! offset (+ offset size))))

  (define (push? opcode)
    (and (symbol? opcode)
         (regexp-match #px"(?i:push)\\d+"
                       (symbol->string opcode))))

  (define (upcase opcode)
    (string->symbol
     (string-upcase
      (symbol->string opcode))))

  (define (instruction/offset is)
    (match is

      ;; instruction
      ;; ---------------------
      ;; (offset hex-string instruction)

      [(list 'label label)
       ;; (label a)
       ;; ---------------------
       ;; (14 "5b" (label a))
       (assert (not (hash-has-key? labels label)))
       (hash-set! labels label offset)
       (list (offset+ (instruction-size 'JUMPDEST))
             (bytes->hex-string
              (bytes-of-opcode 'JUMPDEST))
             is)]

      [(list 'push (list 'label label))
       ;; (push (label a))
       ;; ---------------------
       ;; (5 "61000e" (push 14))
       (list (offset+ 3)
             (λ ()
               (bytes->hex-string
                (bytes-append
                 (bytes-of-opcode 'PUSH2)
                 (integer->bytes (hash-ref labels label)
                                 2 #f))))
             (λ () `(push ,(hash-ref labels label))))]

      [(list (? push? (app upcase op)) addr)
       ;; (PUSH4 #xFFFFFFFF)
       ;; ---------------------
       ;; (9 "63ffffffff" (push4 4294967295))
       (list (offset+ (instruction-size op))
             (bytes->hex-string
              (bytes-append
               (bytes-of-opcode op)
               (integer->bytes addr
                               (push-arg-size op)
                               #f)))
             is)]

      [(list (app upcase op))
       ;; (add)
       ;; ---------------------
       ;; (17 "01" (add))
       (list (offset+ (instruction-size op))
             (bytes->hex-string
              (bytes-of-opcode op))
             is)]))

  (define (patch-label is)
    (match is
      [(list addr (? procedure? patch-bytecode) (? procedure? patch-instruction))
       ;; (5 λ1 λ2)
       ;; ---------------------
       ;; (5 (λ1) (λ2))
       (list addr (patch-bytecode) (patch-instruction))]

      [_ is]))

  (map patch-label
       (map instruction/offset instructions)))

(module+ test
  (displayln "Test 1 instructions/patched-labels")
  (instructions/patched-labels '((push1 #x01)
                                 (push1 #x02)
                                 (add)
                                 (push (label a))
                                 (jump)
                                 (push4 #xffffffff)
                                 (label a)
                                 (push1 1)
                                 (add)
                                 (return))))

(define (assemble instructions)
  (bytes->hex-string
   (apply bytes-append
          (map (match-lambda [(list _ hex _) (hex-string->bytes hex)])
               (instructions/patched-labels instructions)))))

(module+ test
  (displayln "Test 2 assemble")
  (assemble '((push1 #x01)
              (push1 #x02)
              (add)
              (push (label a))
              (jump)
              (push4 #xffffffff)
              (label a)
              (push1 1)
              (add)
              (return))))

(define (disassemble bytes [jump-targets? #f])
  (when (string? bytes)
    (set! bytes (hex-string->bytes bytes)))

  ;; constantly #f
  (define (or-false) #f)

  ;; table: offset of (PUSH2 addr?)  => addr?
  (define jumps (make-hash))
  ;; table: addr => (label (gensym))
  (define targets (make-hash))

  ;; retrieve generated label by offset of its def
  (define (target->label? offset)
    (hash-ref targets offset or-false))

  ;; retrieve generated label by offset of its use
  (define (jump->label? offset)
    (target->label?
     (hash-ref jumps offset or-false)))

  ;; generate bytecode
  (define (asm-of byte-seq offset)
    (if (empty? byte-seq)
        '()
        (let* ((bytes   byte-seq)
               (opbyte  (first bytes))
               (bytes   (rest bytes))
               (opcode  (is-opcode (hash-ref isa-by-bytecode opbyte)))
               (argsize (if (push? opcode) (push-arg-size opcode) 0))
               (arg     (if (positive? argsize)
                            (list
                             (bytes->integer
                              (list->bytes
                               (take bytes argsize))
                              #f))
                            '())))
          (when jump-targets?
            (cond
              ((= argsize 2)             (hash-set! jumps offset (first arg)))
              ((equal? opcode 'JUMPDEST) (hash-set! targets
                                                    offset
                                                    (list 'label (gensym))))))
          (cons (list offset (cons opcode arg))
                (asm-of (drop bytes argsize) (+ offset 1 argsize))))))

  ;; Jump target is is any argument to PUSH2 that allso happens to be an offset in the
  ;; code. This is ambiguous and may result in false labels being generated.
  (define (with-label instruction)
    (match instruction
      [(list-rest offset _)
       (cond
         ((jump->label? offset)   => (λ (label) (list offset (list 'push label))))
         ((target->label? offset) => (λ (label) (list offset label)))
         (else instruction))]))

  (if jump-targets?
      (map with-label (asm-of (bytes->list bytes) 0))
      (asm-of (bytes->list bytes) 0)))

(module+ test
  (displayln "Test 3 disassemble")
  (disassemble
   (assemble '((push1 #x01)
               (push1 #x02)
               (add)
               (push (label a))
               (jump)
               (push4 #xffffffff)
               (label a)
               (push1 1)
               (add)
               (return)))
   #t))

#|
(struct instr (asm
               bytes
               offset
               size
               srcloc
               opcode
               opbyte
               cost)
  #:transparent
  ;; #:guard (λ ()) TODO
  ;; #:methods gen:custom-write TODO
  ;; #:property prop:procedure (case-lambda [(self) (self arg)]) TODO
  ;; #:property prop:procedure
  ;; (λ (self state)
  ;;   (match self
  ;;     [(instruction 'PUSH1 ...) (push1 ...)]
  ;;     [(instruction 'JUMP ...) (jump ...)]))
  )
|#
