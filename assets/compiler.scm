;;#lang racket
;; vectorlisp: a strange language for procedural rendering

(define debug #f)

(define nop 0) (define jmp 1) (define jmz 2) (define jlt 3) (define jgt 4)
(define ldl 5) (define lda 6) (define ldi 7) (define sta 8) (define sti 9)
(define add 10) (define sub 11) (define mul 12) (define div 13) (define abs 14)
(define _sin 15) (define atn 16) (define dot 17) (define crs 18) (define sqr 19)
(define len 20) (define dup 21) (define drp 22) (define cmp 23) (define shf 24)
(define bld 25) (define ret 26) (define _dbg 27) (define nrm 28)
(define add.x 29) (define add.y 30) (define add.z 31) (define end-check 999)
(define swp 32) (define rnd 33) (define mull 34) (define jmr 35) (define ldlv 36)
(define lensq 37) (define noise 38) (define lds 39) (define sts 40) (define mulv 41)

(define instr 
  '(nop jmp jmz jlt jgt ldl lda ldi sta sti
        add sub mul div abs _sin atn dot crs 
        sqr len dup drp cmp shf bld ret dbg 
        nrm add.x add.y add.z swp rnd mull
        jmr ldlv lensq noise lds sts mulv))

(define prim-triangles 0)
(define prim-tristrip 1)
(define prim-points 2)
(define prim-lines 3)
(define prim-linestrip 4)

(define reg-fling 3)
(define emit list)

;; variables are just an address lookup table
(define variables '())

(define (make-variable! name)
  (when (not (memq name variables))
        (set! variables (append variables (list name)))))

(define (variable-address name)
  (define (_ l c)   
    (cond
     ((null? l) (msg "cant find variable " name) #f)
     ((equal? name (car l)) c)
     (else (_ (cdr l) (+ c 1)))))
  ;; 4 is starting address after registers
  (_ variables 4)) 

;; variables are just an address lookup table
(define function-code '())

;; returns address relative to start of function block
(define (make-function! code)
  (let ((addr (length function-code)))
    (set! function-code (append function-code code))
    addr))

;; segments are data areas, positions, normals, colours etc
(define segment-size 512)

(define (memseg n) (* segment-size n))

(define (get-segment name)
  (cond
   ((eq? name 'code-start) (memseg 0))
   ((eq? name 'code-end) (memseg 1))
   ((eq? name 'positions-start) (memseg 1))
   ((eq? name 'positions-end) (memseg 2))
   ((eq? name 'normals-start) (memseg 2))
   ((eq? name 'normals-end) (memseg 3))
   ((eq? name 'colours-start) (memseg 3))
   ((eq? name 'colours-end) (memseg 4))
   ((eq? name 'texture-start) (memseg 4))
   ((eq? name 'texture-end) (memseg 5))
   (else #f)))

;; when we find a symbol, look it up
(define (emit-constant-or-variable name)
  (let ((seg (get-segment name)))
    (cond
     ;; if a memory segment constant is found
     (seg (emit (vector ldl seg 0)))
     ;; other constants
     ((eq? name 'reg-fling) (emit (vector ldl 3 0)))
     ;; load variable
     (else 
      (emit (vector lda (variable-address name) 0))))))
  
;; create empty space for variable data
(define (variables->data)
  (map
   (lambda (i)
     (vector 0 0 0))
   variables))

;; is this an immediate value
(define (immediate? x)
  (or (number? x) (symbol? x) (eq? (car x) 'vector)))

;; push whatever this immediate value is onto the stack
(define (emit-push x)
  (cond
   ((number? x) (emit (vector ldl x 0)))
   ((vector? x) (emit (vector ldlv 0 0) x))
   ((symbol? x) (emit-constant-or-variable x))
   ((list? x) (emit (vector ldlv 0 0) (list->vector (cdr x))))
   (else
    (error "can't push" x))))

;; is this a primitive call?
(define (primcall? x)
  (and (list? x) (not (null? x)) (symbol? (car x))))

;; append a bunch of expressions
(define (emit-expr-list l)
  (cond 
    ((null? l) '())
    (else 
     (append (emit-expr (car l)) 
             (emit-expr-list (cdr l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; primitive function calls follow

(define (emit-set! x)
  (append
   (emit-expr (caddr x))
   (emit 
    (vector sta (variable-address (cadr x)) 0))))

(define (emit-write! x)
  (append
   (emit-expr (caddr x)) ;; data
   (emit-expr (cadr x)) ;; address
   (emit (vector sts 0 0))))

(define (emit-read x)
  (append
   (emit-expr (cadr x)) ;; address
   (emit (vector lds 0 0))))

(define (emit-cond-part x)
  (let ((block (emit-expr-list (cdr x))))
    (append 
     (emit-expr (car x))
     (emit (vector jmz (+ (length block) 1) 0))
     block)))

(define (emit-cond x)
  (define (_ l)
    (cond
     ((null? l) '())
     (else (append (emit-cond-part (car l)) 
                   (_ (cdr l))))))
  (_ (cdr x)))

(define (emit-fncall x addr)
  (let ((args (emit-expr-list (cdr x))))
    (append
     ;; offset from here -> stitch up in second pass
     (emit (list 'add-abs-loc 'this 1 
                 (vector ldl (+ (length args) 3) 0))) 
     args ;; push arguments to stack
     (emit (vector lda addr 0)) ;; fn ptr is in data mem
     (emit (vector ret 0 0))))) ;; jump to fn

;; lambda args body
(define (emit-lambda x)
  (let* ((body 
          (append
           (map 
            (lambda (arg)
              ;; for moment use global pile for arguments :O
              (make-variable! arg)
              (vector sta (variable-address arg) 0))
            (cadr x))
           ;; now args are loaded, do body
           (emit-expr-list (cddr x))
           ;; swap ret ptr to top
           (emit (vector swp 0 0)) 
           (emit (vector ret 0 0))))
         (loc (make-function! body)))
    (append
     (if debug (emit (list "function code...")) '())
     (emit 
      ;; offset from function code -> stitch up in linking pass
       (list 'add-abs-loc 'function-code 1 
            (vector ldl loc 0))))))
   
(define (emit-define x)
  (make-variable! (cadr x))
  (append
   (emit-expr (caddr x))
   (emit (vector sta (variable-address (cadr x)) 0))))

(define (emit-let-part x)
  (make-variable! (car x))
  (append
   (emit-expr (cadr x))
   (emit (vector sta (variable-address (car x)) 0))))

(define (emit-let x)
  (define (_ l)
    (cond
     ((null? l) '())
     (else (append (emit-let-part (car l)) 
                   (_ (cdr l))))))
  (append
   (_ (cadr x))
   (emit-expr-list (cdr (cdr x)))))

(define (emit-trace x)
  (append
   (emit-expr (cadr x))
   (emit (vector _dbg 0 0))))

(define (emit-not x)
  (append
   (emit-expr (cadr x))
   (emit (vector jmz 3 0))
   (emit (vector ldl 0 0))
   (emit (vector jmr 2 0))
   (emit (vector ldl 1 0))))

;(loop pred block)
(define (emit-loop x)
  (let ((block 
         (append
          (emit-expr-list (cdr (cdr x)))
          (emit-expr (cadr x)))))
    (append
     block
     (emit (vector jmz 2 0))
     (emit (vector jmr (- (+ (length block) 1)) 0))
     )))

(define (binary-procedure proc x) 
  (append
   (emit-expr (cadr x))
   (emit-expr (caddr x))
   (emit (vector proc 0 0))))

(define (emit-eq? x)
  (append
   (emit-expr (cadr x))
   (emit-expr (caddr x))
   (emit (vector sub 0 0))
   (emit (vector jmz 3 0))
   (emit (vector ldl 0 0))
   (emit (vector jmr 2 0))
   (emit (vector ldl 1 0))))

(define (emit-< x)
  (append
   (emit-expr (cadr x))
   (emit-expr (caddr x))
   (emit (vector jlt 3 0))
   (emit (vector ldl 1 0))
   (emit (vector jmr 2 0))
   (emit (vector ldl 0 0))))

(define (emit-> x)
  (append
   (emit-expr (cadr x))
   (emit-expr (caddr x))
   (emit (vector jgt 3 0))
   (emit (vector ldl 1 0))
   (emit (vector jmr 2 0))
   (emit (vector ldl 0 0))))

(define (emit-swizzle x)
  (append
   (emit-expr (caddr x))
   (emit (vector ldlv 0 0))
   (cond 
    ((eq? (cadr x) 'xxx) (emit (vector 0 0 0)))
    ((eq? (cadr x) 'xxy) (emit (vector 0 0 1)))  
    ((eq? (cadr x) 'xxz) (emit (vector 0 0 2)))
    ((eq? (cadr x) 'xyx) (emit (vector 0 1 0)))
    ((eq? (cadr x) 'xyy) (emit (vector 0 1 1)))
    ((eq? (cadr x) 'xyz) (emit (vector 0 1 2)))
    ((eq? (cadr x) 'xzx) (emit (vector 0 2 0)))
    ((eq? (cadr x) 'xzy) (emit (vector 0 2 1)))
    ((eq? (cadr x) 'xzz) (emit (vector 0 2 2)))
    ((eq? (cadr x) 'yxx) (emit (vector 1 0 0)))
    ((eq? (cadr x) 'yxy) (emit (vector 1 0 1)))
    ((eq? (cadr x) 'yxz) (emit (vector 1 0 2)))
    ((eq? (cadr x) 'yyx) (emit (vector 1 1 0)))
    ((eq? (cadr x) 'yyy) (emit (vector 1 1 1)))
    ((eq? (cadr x) 'yyz) (emit (vector 1 1 2)))
    ((eq? (cadr x) 'yzx) (emit (vector 1 2 0)))
    ((eq? (cadr x) 'yzy) (emit (vector 1 2 1)))
    ((eq? (cadr x) 'yzz) (emit (vector 1 2 2)))
    ((eq? (cadr x) 'zxx) (emit (vector 2 0 0)))
    ((eq? (cadr x) 'zxy) (emit (vector 2 0 1)))
    ((eq? (cadr x) 'zxz) (emit (vector 2 0 2)))
    ((eq? (cadr x) 'zyx) (emit (vector 2 1 0)))
    ((eq? (cadr x) 'zyy) (emit (vector 2 1 1)))
    ((eq? (cadr x) 'zyz) (emit (vector 2 1 2)))
    ((eq? (cadr x) 'zzx) (emit (vector 2 2 0)))
    ((eq? (cadr x) 'zzy) (emit (vector 2 2 1)))
    ((eq? (cadr x) 'zzz) (emit (vector 2 2 2))))
   (emit (vector shf 0 0))))

(define (emit-procedure x)
  (cond
    ((eq? (car x) '+) (binary-procedure add x))
    ((eq? (car x) '-) (binary-procedure sub x))
    ((eq? (car x) '*) (binary-procedure mul x))
    ((eq? (car x) '/) (binary-procedure div x))
    ((eq? (car x) '*v) (binary-procedure mulv x))
    ((eq? (car x) 'cross) (binary-procedure crs x))
    ((eq? (car x) 'dot) (binary-procedure dot x))
    ((eq? (car x) 'eq?) (emit-eq? x))
    ((eq? (car x) '>) (emit-> x))
    ((eq? (car x) '<) (emit-< x))
    ((eq? (car x) 'set!) (emit-set! x))
    ((eq? (car x) 'write!) (emit-write! x))
    ((eq? (car x) 'swizzle) (emit-swizzle x))
    ((eq? (car x) 'lambda) (emit-lambda x))
    ((eq? (car x) 'rndvec) (emit (vector rnd 0 0)))
    ((eq? (car x) 'trace) (emit-trace x))
    ((eq? (car x) 'read) (emit-read x))
    ((eq? (car x) 'not) (emit-not x))
    ((eq? (car x) 'add1)
     (append
      (emit-expr (cadr x))
      (emit-push (vector 1 0 0))
      (emit (vector add 0 0))))
    ((eq? (car x) 'sub1)
     (append
      (emit-expr (cadr x))
      (emit-push (vector 1 0 0))
      (emit (vector sub 0 0))))
    ((eq? (car x) 'mag)
     (append
      (emit-expr (cadr x))
      (emit (vector len 0 0))))
    ((eq? (car x) 'magsq)
     (append
      (emit-expr (cadr x))
      (emit (vector lensq 0 0))))
    ((eq? (car x) 'noise)
     (append
      (emit-expr (cadr x))
      (emit (vector noise 0 0))))
    ((eq? (car x) 'normalise)
     (append
      (emit-expr (cadr x))
      (emit (vector nrm 0 0))))
    ((eq? (car x) 'ignore)
     (append
      (emit-expr (cadr x))
      (emit (vector drp 0 0))))
    (else 
     (let ((addr (variable-address (car x))))
       (if addr
           (emit-fncall x addr)
           (begin
             (msg "unknown function: " x) '()))))))

(define (emit-expr x)
  (cond 
   ((immediate? x) (emit-push x))
   ((primcall? x)
    (append
     (if debug 
         (list (string-append "starting " (symbol->string (car x))))
         '())     
     (cond 
      ((eq? (car x) 'let) (emit-let x))
      ((eq? (car x) 'define) (emit-define x))
      ((eq? (car x) 'cond) (emit-cond x))
      ((eq? (car x) 'loop) (emit-loop x))
      (else (emit-procedure x)))
     (if debug
         (list (string-append "ending " (symbol->string (car x))))
         '())
     ))
   (else 
    (msg "don't understand " x) '())))

(define (header code-start cycles prim hints)
  (list
   (vector code-start cycles 0) ;; control (pc, cycles, stack)
   (vector 512 prim hints) ;; graphics
   (vector 0 0 0) ;; pos
   (vector 0 0 0))) ;; sensor

(define (link-add code addr)
  (let ((v (list-ref code 3)))
    (vector-set! v (list-ref code 2)
                 (+ (vector-ref v (list-ref code 2)) 
                    addr))
    v))

(define (link-dangling code function-start pc)
  (cond
   ((eq? (car code) 'add-abs-loc)
    (cond
     ((eq? (cadr code) 'this) 
      (link-add code pc))
     ((eq? (cadr code) 'function-code) 
      (link-add code function-start))
     (else 
      (msg "link offset unhandled:" code))))
   (else
    (msg "link type unhandled:" code))))

(define (link-program code function-start pc)
  (cond
   ((null? code) '())
   ((list? (car code))
    (cons
     (link-dangling (car code) function-start pc)
     (link-program (cdr code) function-start (+ pc 1))))
   (else
    (cons
     (car code) 
     (link-program (cdr code) function-start (+ pc 1))))))
           
(define (compile-program cycles prim hints x)
  (let* ((code (emit-expr x))
         (data (variables->data))
         (function-start (+ 4 (length data)))
         (out 
          (link-program
           (append
            (header (+ 4 (length data) 
                       (length function-code)) 
                    cycles prim hints)
            data
            function-code
            code)
           function-start 0)))
    ;(msg "code is as follows") 
    ;(disassemble out)
    out))

(define (disassemble code)
  (define ln 0)
  (for-each
   (lambda (i)
     (display ln)(display ": ")
     (cond 
      ((vector? i)
       (if (and (>= (vector-ref i 0) 0) 
                (< (vector-ref i 0) (length instr)))
           (display (list-ref instr (inexact->exact (round (vector-ref i 0)))))
           (display (vector-ref i 0)))
       (display " ")(display (vector-ref i 1))
       (display " ")(display (vector-ref i 2))(newline))
      ((symbol? i) (display (string-append ";; " (symbol->string i)))(newline))
      (else (display ";; ")(display i)(newline)))
     (set! ln (+ ln 1)))
   code))

