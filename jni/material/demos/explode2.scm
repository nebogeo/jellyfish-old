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
(define sin 15)
(define atn 16)
(define dot 17)
(define crs 18)
(define sqr 19)
(define len 20)
(define dup 21)
(define cmp 22)
(define shf 23)
(define bld 24)
(define ret 25)
(define dbg 26)
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

(define (jelly-prog ins-per-frame l)
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
      (pdata-set! "p" reg-ins (vector ins-per-frame 0 0))
      (define addr 0)
      (for-each
       (lambda (v)
         (pdata-set! "p" addr v)
         (set! addr (+ addr 1)))
       (cadr p)))
     (else (display "end check wrong ") (display p) (newline)))))

(clear)
(clear-colour (vector 1 1 1))
;(define p (build-cube))
(define jelly (build-jellyfish))

(define explode
  (list
   jmp 4 0           ;; goto code
   reg-mdl 0 0       ;; 1 address of current vertex
   reg-mdl 0 0       ;; 2 address of accum vertex (loop)
   0 0 0             ;; 3 influence

   ;; accum-loop:
   ldi  1  0         ;; 4 load current vertex
   ldi  2  0         ;; 5 load accum vertex
   sub  0  0         ;; 6 get the difference
   lda  3  0         ;; 7 load the accumulation
   add  0  0         ;; 8 accumulate
   sta  3  0         ;; 9 store accumulation
   ;; inc address
   ldl  1  0         ;; 10 load address add
   lda  2  0         ;; 11 load accum address
   add  0  0         ;; 12 add them
   sta  2  0         ;; 13 store the incremented address
   ;; loop section
   lda  2  0         ;; 15 load accum address
   ldl reg-mdl-end 0 ;; 14 load end address
   jgt  2  0         ;; 16 exit if greater than model end (relative address)
   jmp  4  0         ;; 17 accum-loop
   ;; end accum loop

   ;; normalise
   lda  3  0           ;; 18 load accum
   dup  0  0           ;; 19
   len  0  0           ;; 20 get the length
   div  0  0           ;; 21 normalise it

   ;; make small
   ldl 0.1 0           ;; 22
   mul 0 0             ;; 23

   lda 1 0   ;; load current vertex address
   ldl 1 0   ;; subtract
   sub 0 0   ;; one
   sta 202 0 ;; store far away
   ldi 202 0 ;; load previous
   ldi 1 0   ;; load current
   sub 0 0   ;; get the difference
   ldl 0.2 0
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

   lda 1 0           ;; 31 load current vertex address
   ldl 1 0           ;; 32
   add 0 0           ;; 33 increment
   sta 1 0           ;; 34 store

   lda 1  0         ;; 36 load vertex address
   ldl reg-mdl-end 0 ;; 35 load end address
   jgt 2  0          ;; 37 if greater than model (relative address)
   jmp 4  0           ;; 38

   ;; reset current vertex
   ldl reg-mdl 0
   sta 1 0

   jmp 4 0
   end-check))

(define twirl
  (list
   0 0 0   ;; time (increases by 1 each loop)
   2 2 -3  ;; shuffle data for converting (x y z) -> (z z x)
   ;; code follows to build a vertex by rotation around an angle based on the index
   lda 0 0       ;; load current time from address 0
   ldl 135.3 0   ;; load angle 135.3 (in degrees)
   mul 0 0       ;; multiply time by angle
   sin 0 0       ;; makes (sin(angle) cos(angle) 0)
   ;; make a spiral by scaling up with time
   lda 0 0       ;; load time again
   ldl 0.05 0    ;; load 0.05
   mul 0 0       ;; multiply to get time*0.05
   mul 0 0       ;; mul rotation vector by time*0.05
   ;; move backward in z so we get some depth
   lda 0 0       ;; load the time again
   ldl 0.03 0    ;; load 0.03
   mul 0 0       ;; multiply the time by 0.01
   lda 1 0       ;; load the shuffle vec from address 1
   shf 0 0       ;; shuffle the x to z position
   add 0 0       ;; add (0 0 x) to set z on current position
   sti 0 reg-mdl ;; write position to model memory registers
   ;; increment the index by 1
   lda 0 0       ;; load address
   ldl 1 0       ;; load inc
   add 0 0       ;; add them together
   sta 0 0       ;; store at address loc
   jmp 2 0       ;; goto 2
   end-check   ))

(with-primitive
 jelly

 (pdata-map!
  (lambda (p)
    (srndvec)) ;(vector 0.7 0.7 0.7 0.4))
  "p")

 (jelly-prog 2000 twirl)

 (pdata-map!
  (lambda (c)
    (rndvec)) ;(vector 0.7 0.7 0.7 0.4))
  "c")
 (hint-unlit)
 (hint-wire)
 (line-width 3))


(with-primitive
 jelly
 (translate (vector -0.3 1.4 0))
 (rotate (vector -40 -20 2))
 )

                                        ;(with-state
; (rotate (vector 45 45 0))
; (build-cube))

;(every-frame
; (with-primitive jelly (rotate (vector 2.2 2 1))))
