;;#lang racket
;; vectorlisp: a strange language for procedural rendering

(define nop 0) (define jmp 1) (define jmz 2) (define jlt 3) (define jgt 4)
(define ldl 5) (define lda 6) (define ldi 7) (define sta 8) (define sti 9)
(define add 10) (define sub 11) (define mul 12) (define div 13) (define abs 14)
(define _sin 15) (define atn 16) (define dot 17) (define crs 18) (define sqr 19)
(define len 20) (define dup 21) (define drp 22) (define cmp 23) (define shf 24)
(define bld 25) (define ret 26) (define _dbg 27) (define nrm 28)
(define add.x 29) (define add.y 30) (define add.z 31) (define end-check 999)
(define swp 32) (define rnd 33) (define mull 34) (define jmr 35) (define ldlv 36)
(define lensq 37)

(define instr 
  '(nop jmp jmz jlt jgt ldl lda ldi sta sti
        add sub mul div abs _sin atn dot crs 
        sqr len dup drp cmp shf bld ret dbg 
        nrm add.x add.y add.z swp rnd mull
        jmr ldlv lensq))

(define prim-triangles 0)
(define prim-tristrip 1)
(define prim-points 2)
(define prim-lines 3)
(define prim-linestrip 4)

(define emit list)

(define (emit-label label)
  (printf "~a:~n" label))

;; make labels for jmps
(define next-label-id 0)
(define (unique-label)
  (set! next-label-id (add1 next-label-id))
  (string-append "label-" (number->string next-label-id)))

(define variables '())

(define (make-variable! name)
  (set! variables (append variables (list name))))

(define (variable-address name)
  (define (_ l c)   
    (cond
     ((null? l) (msg "cant find variable " name) #f)
     ((equal? name (car l)) c)
     (else (_ (cdr l) (+ c 1)))))
  (_ variables 4))

(define (variables->data)
  (map
   (lambda (i)
     (vector 0 0 0))
   variables))

(define (immediate? x)
  (or (number? x) (symbol? x) (eq? (car x) 'vector)))

(define (emit-push x)
  (cond
   ((number? x) (emit (vector ldl x 0)))
   ((vector? x) (emit (vector ldlv 0 0) x))
   ((symbol? x) (emit (vector lda (variable-address x) 0)))
   ((list? x) (emit (vector ldlv 0 0) (list->vector (cdr x))))
   (else
    (error "can't push" x))))

(define (primcall? x)
  (and (list? x) (not (null? x)) (symbol? (car x))))

(define (emit-expr-list l)
  (cond 
    ((null? l) '())
    (else 
     (append (emit-expr (car l)) 
             (emit-expr-list (cdr l))))))

(define (emit-set! x)
  (append
   (emit-expr (caddr x))
   (emit (vector sta (variable-address (cadr x)) 0))))

(define (emit-write! x)
  (append
   (emit-expr (caddr x))
   (emit (vector sti (variable-address (cadr x)) 0))))

(define (emit-read x)
  (emit (vector ldi (variable-address (cadr x)) 0)))

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

(define (emit-unary-procedure x)
  (cond
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
   ((eq? (car x) 'normalise)
    (append
     (emit-expr (cadr x))
     (emit (vector nrm 0 0))))
   (else
    (msg "procedure not understood: " (car x)))))

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

;; just uses betablocker's internal stack
(define (emit-procedure x)
  (cond
    ((eq? (car x) '+) (binary-procedure add x))
    ((eq? (car x) '-) (binary-procedure sub x))
    ((eq? (car x) '*) (binary-procedure mul x))
    ((eq? (car x) '/) (binary-procedure div x))
    ((eq? (car x) 'cross) (binary-procedure crs x))
    ((eq? (car x) 'dot) (binary-procedure dot x))
    ((eq? (car x) 'eq?) (emit-eq? x))
    ((eq? (car x) '>) (emit-> x))
    ((eq? (car x) '<) (emit-< x))
    ((eq? (car x) 'set!) (emit-set! x))
    ((eq? (car x) 'write!) (emit-write! x))
    (else (display x) (newline) '())))

(define (emit-expr x)
  (cond 
   ((immediate? x)
    (emit-push x))
   ((primcall? x)
    (append
     (list (list "start " (car x)))
     (cond 
      ((eq? (car x) 'let) (emit-let x))
      ((eq? (car x) 'cond) (emit-cond x))
      ((eq? (car x) 'loop) (emit-loop x))
      ((eq? (car x) 'rndvec) (emit (vector rnd 0 0)))
      ((eq? (car x) 'trace) (emit-trace x))
      ((eqv? (length x) 2) (emit-unary-procedure x))
      (else (emit-procedure x)))
     (list (list "end " (car x)))
     ))
   (else 
    (msg "don't understand " x) '())))

(define (header code-start cycles prim)
  (list
   (vector code-start cycles 0) ;; control (pc, cycles, stack)
   (vector 512 prim 1) ;; graphics
   (vector 0 0 0) ;; pos
   (vector 0 0 0))) ;; sensor

(define (compile-program cycles prim x)
  (let ((code (emit-expr x))
        (data (variables->data)))
    (append
     (header (+ 4 (length data)) cycles prim)
     data
     code)))

(define (disassemble code)
  (for-each
   (lambda (i)
     (cond 
      ((vector? i)
       (if (< (vector-ref i 0) (length instr))
           (display (list-ref instr (vector-ref i 0)))
           (display (vector-ref i 0)))
       (display " ")(display (vector-ref i 1))
       (display " ")(display (vector-ref i 2))(newline))
      ((symbol? i) (display (string-append ";; " (symbol->string i)))(newline))
      (else (display ";; ")(display i)(newline))))
   code))

