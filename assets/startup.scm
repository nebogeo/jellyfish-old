(define nop 0)
(define jmp 1)
(define jmz 2)
(define jlt 3)
(define jgt 4)
(define ldl 5)
(define lda 6)
(define ldi 7)
(define sta 8)
(define sti 9)
(define add 10)
(define sub 11)
(define mul 12)
(define div 13)
(define abs 14)
(define _sin 15)
(define atn 16)
(define dot 17)
(define crs 18)
(define sqr 19)
(define len 20)
(define dup 21)
(define drp 22)
(define cmp 23)
(define shf 24)
(define bld 25)
(define ret 26)
(define dbg 27)
(define nrm 28)
(define add.x 29)
(define add.y 30)
(define add.z 31)
(define end-check 999)

(define reg-pco 100)
(define reg-spd 101)
(define reg-pos 102)
(define reg-vel 103)
(define reg-col 104)
(define reg-nit 105)
(define reg-scc 106)
(define reg-sdr 107)
(define reg-and 108)
(define reg-ins 109)
(define reg-mdl 120)
(define reg-mdl-end 199)
(define reg-stp 200)
(define reg-stk 201)
(define reg-ndt 256)

(define (jelly-prog l)
  (let ((p (foldl
            (lambda (i r)
              (let ((cur (car r))
                    (l (cadr r)))
                (if (eqv? (length cur) 3)
                    (list (list i) (append l (list (list->vector cur))))
                    (list (append cur (list i)) l))))
            (list '() '()) l)))
    (cond
     ((eq? (car (car p)) end-check)
      (define addr 0)
      (for-each
       (lambda (v)
         (pdata-set! "x" addr v)
         (set! addr (+ addr 1)))
       (cadr p)))
     (else (display "end check wrong ") (display p) (newline)))))


(define explode
  (list
   jmp 4 0           ;; goto code
   reg-mdl 0 0       ;; 1 address of current vertex
   reg-mdl 0 0       ;; 2 address of accum vertex (loop)
   0 0 0             ;; 3 influence

   ;; accum-loop:
   ldi  2  0         ;; 4 load current vertex
   ldi  1  0         ;; 5 load accum vertex
   sub  0  0         ;; 6 get the difference
   lda  3  0         ;; 7 load the accumulation
   add  0  0         ;; 8 accumulate
   nrm  0  0         ;; normalise

   sta  3  0         ;; 9 store accumulation

   ;; inc address
   add.x 2 1         ;; 10 load address add

   ;; loop section
   lda  2  0         ;; 15 load accum address
   ldl reg-mdl-end 0 ;; 14 load end address
   jlt  2  0         ;; 16 exit if greater than model end (relative address)
   jmp  4  0         ;; 17 accum-loop
   ;; end accum loop

   ;; normalise
   lda  3  0           ;; 18 load accum

   ;; make small
   ldl 0.1 0
   mul 0 0

   ldl 1 0   ;; subtract
   lda 1 0   ;; load current vertex address
   sub 0 0   ;; one
   sta 80 0 ;; store far away
   ldi 1 0   ;; load current
   ldi 80 0 ;; load previous
   sub 0 0   ;; get the difference
   ldl 0.5 0
   mul 0 0   ;; make small

   add 0 0   ;; add to accum result

   ;; do the animation
   ldi 1 0           ;; 24 load current vertex
   add 0 0           ;; 25 add the accum
   sti 1 0           ;; 26 write to model

   ;; reset the outer loop
   ldl 0 0           ;; 27
   sta 3 0           ;; 28 reset accum
   ldl reg-mdl 0     ;; 29
   sta 2 0           ;; 30 reset accum address

   add.x 1 1         ;; inc vertex address

   lda 1  0         ;; 36 load vertex address
   ldl reg-mdl-end 0 ;; 35 load end address
   jlt 2  0          ;; 37 if greater than model (relative address)
   jmp 4  0           ;; 38

   ;; reset current vertex
   ldl reg-mdl 0
   sta 1 0

   drp 0 0
   drp 0 0

   jmp 4 0
   end-check))

(define twirl
  (list
   jmp 4 0
   0 0 0   ;; time (increases by 1 each loop)
   2 2 -3  ;; shuffle data for converting (x y z) -> (z z x)
   200 0 0
   ;; code follows to build a vertex by rotation around an angle based on the index
   lda 1 0       ;; load current time from address 0
   lda 3 0   ;; load angle 135.3 (in degrees)
   mul 0 0       ;; multiply time by angle
   _sin 0 0       ;; makes (sin(angle) cos(angle) 0)
   ;; make a spiral by scaling up with time
   lda 1 0       ;; load time again
   ldl 0.025 0    ;; load 0.05
   mul 0 0       ;; multiply to get time*0.05
   mul 0 0       ;; mul rotation vector by time*0.05
   ;; move backward in z so we get some depth
   lda 1 0       ;; load the time again
   ldl 0.03 0    ;; load 0.03
   mul 0 0       ;; multiply the time by 0.01
   lda 2 0       ;; load the shuffle vec from address 1
   shf 0 0       ;; shuffle the x to z position
   add 0 0       ;; add (0 0 x) to set z on current position

   ldl 0.1 0
   mul 0 0
   ldi 1 reg-mdl
   ldl 0.9 0
   mul 0 0
   add 0 0
   sti 1 reg-mdl ;; write position to model memory registers
   ;; increment the index by 1
   add.x 1 1
   ldl (- reg-mdl-end reg-mdl) 0
   lda 1 0
   jgt 2 0
   jmp 4 0       ;; goto 2
   ldl 0 0
   sta 1 0
   jmp 4 0
   end-check   ))


(define v2-test
  (list
   10 1000 0 ;; control (pc, cycles, stack)
   512 0 0 ;; graphics
   0 0 0 ;; pos
   0 0 0 ;; sensor

   0 0 0 ;; space for data (4:t)
   100 0 0 ;; 5:angle
   2 2 -3 ;; 6:shuffle
   0 0 0
   0 0 0
   0 0 0

   ;; code follows to build a vertex by rotation around an angle based on the index
   lda 4 0       ;; load current time from address 0
   lda 5 0   ;; load angle 135.3 (in degrees)
   mul 0 0       ;; multiply time by angle
   _sin 0 0       ;; makes (sin(angle) cos(angle) 0)
   ;; make a spiral by scaling up with time
   lda 4 0       ;; load time again
   ldl 0.025 0    ;; load 0.05
   mul 0 0       ;; multiply to get time*0.05
   mul 0 0       ;; mul rotation vector by time*0.05
   ;; move backward in z so we get some depth
   lda 4 0       ;; load the time again
   ldl 0.03 0    ;; load 0.03
   mul 0 0       ;; multiply the time by 0.01
   lda 6 0       ;; load the shuffle vec from address 1
   shf 0 0       ;; shuffle the x to z position
   add 0 0       ;; add (0 0 x) to set z on current position

   sti 4 512 ;; write position to model memory registers
   ;; increment the index by 1
   add.x 4 1
   lda 1 0 ;; load graphics reg (z is size)
   lda 4 0
   jgt 2 0
   jmp 10 0
   ldl 0 0
   sta 4 0
   jmp 10 0
   end-check
   ))

(clear)
(clear-colour (vector 0 0.2 0.5))
;(define p (build-cube))
(define jellys (build-list (lambda (i) (build-jellyfish 512)) 1))

(for-each
 (lambda (j)
   (with-primitive
    j
    (pdata-index-map!
     (lambda (i p)
       (crndvec))
     "p")

    (jelly-prog v2-test)

    (pdata-index-map!
     (lambda (i c)
       (let ((ir (modulo (* i 4) (pdata-size)))
             (ig (modulo (* i 20) (pdata-size)))
             (ib (modulo (* i 3) (pdata-size))))
         (vector (/ ir (pdata-size))
                 (/ ig (pdata-size))
                 (/ ib (pdata-size)))))
     "c")
    (hint-unlit)
    (hint-wire)
    (line-width 3)
    ;(let ((p (srndvec)))
    ;  (translate (vector (* (vx p) 2)
    ;                     (* (vy p) 4)
    ;                     (* (vz p) 20))))

    ))
 jellys)



(define current 0)
(define frames 100)
(define frame 0)

(set! on-sensor
      (lambda (x y z)
        (with-primitive
         (list-ref jellys current)
         (pdata-set! "p" 3 (vmul (vector x y z) 5)))))

;(every-frame
; (begin
;   (when (> frame frames)
;         (for-each
;          (lambda (j)
;            (with-primitive
;             j (pdata-set! "p" reg-ins (vector 0 0 0))))
;          jellys)
;
;         (set! current (modulo (+ current 1) (length jellys)))
;
;         (with-primitive
;          (list-ref jellys current)
;          (pdata-set! "p" reg-ins (vector 1000 0 0)))
;         (set! frame 0))
;   (set! frame (+ frame 1))))

;(define y (build-cube))
;(with-primitive
; y
; (display (pdata-ref "p" 0))(newline))
;(every-frame (with-primitive y (rotate (vector 1 2 3))))
