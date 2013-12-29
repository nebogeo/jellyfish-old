;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(display "jellyfish.scm")(newline)

(define nop 0) (define jmp 1) (define jmz 2) (define jlt 3) (define jgt 4)
(define ldl 5) (define lda 6) (define ldi 7) (define sta 8) (define sti 9)
(define add 10) (define sub 11) (define mul 12) (define div 13) (define abs 14)
(define _sin 15) (define atn 16) (define dot 17) (define crs 18) (define sqr 19)
(define len 20) (define dup 21) (define drp 22) (define cmp 23) (define shf 24)
(define bld 25) (define ret 26) (define _dbg 27) (define nrm 28)
(define add.x 29) (define add.y 30) (define add.z 31) (define end-check 999)
(define swp 32) (define rnd 33) (define mull 34) (define jmr 35) (define ldlv 36)

(define prim-triangles 0)
(define prim-tristrip 1)
(define prim-points 2)
(define prim-lines 3)
(define prim-linestrip 4)

;(define (hint-none) (hint 0))
;(define (hint-solid) (hint 1))
;(define (hint-wire) (hint 2))
;(define (hint-normal) (hint 3))
;(define (hint-points) (hint 4))
;(define (hint-anti-alias) (hint 5))
;(define (hint-bound) (hint 6))
;(define (hint-unlit) (hint 7))


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

(define (jelly-compiled code)
  (define addr 0)
  (for-each
   (lambda (v)
     (pdata-set! "x" addr v)
     (set! addr (+ addr 1)))
   code))

(define v2-test
  (list
   10 5000 0 ;; control (pc, cycles, stack)
   512 prim-linestrip 1 ;; graphics
   0 0 0 ;; pos
   0 0 0 ;; sensor

   0 0 0 ;; space for data (4:t)
   100 0 0 ;; 5:angle
   2 2 -3 ;; 6:shuffle
   0 0 0  ;; 7:time
   0 0 0
   0 0 0

   ;; code follows to build a vertex by rotation around an angle based on the index
   lda 4 0       ;; load current time from address 0
   lda 7 0
   mul 0 0
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
   add.x 7 0.002
   jmp 10 0
   end-check
   ))

(define mdl-size 512)
(define mdl-start mdl-size)
(define mdl-end (+ mdl-size 20))

(define banner
  (list
   ;; register section
   8 20000 0 ;; control (pc, cycles, stack)
   mdl-size prim-tristrip 1 ;; graphics (size, primtype, renderhints)
   0 0 0 ;; pos
   0 0 0 ;; sensor addr

   ;; program data section
   mdl-start 0 0     ;; 4 address of current vertex
   mdl-start 0 0     ;; 5 address of accum vertex (loop)
   0 0 0             ;; 6 influence
   0 0 0             ;; temp

   ;; code section 
   ;; add up differences with every other vertex
   ldi  4  0         ;; load current vertex
   ldi  5  0         ;; load accum vertex
   sub  0  0         ;; get the difference
   lda  6  0         ;; load the accumulation
   add  0  0         ;; accumulate
   nrm  0  0         ;; normalise
   sta  6  0         ;; store accumulation
   add.x 5 1         ;; inc accum address

   ;; accumulation iteration 
   lda  5  0         ;; load accum address
   ldl  mdl-end 0    ;; load end address
   jlt  2  0         ;; exit if greater than model end (relative address)
   jmp  8  0         ;; return to accum-loop

   ;; end accum loop
   ;; push away from other verts
   lda  6  0         ;; load accum
   ldl -0.1 0        ;; reverse & make smaller
   mul 0 0

   ;; attract to next vertex
   ldi 4 0           ;; load current
   ldi 4 1           ;; load next
   sub 0 0           ;; get the difference
   ldl 0.4 0
   mul 0 0           ;; make smaller
   add 0 0           ;; add to accum result

   ;; do the animation
   ldi 4 0           ;; load current vertex
   add 0 0           ;; add the accumulation
   sti 4 0           ;; write to model data

   ;; reset the outer loop
   ldl 0 0           ;; load zero
   sta 6 0           ;; reset accum
   ldl mdl-start 0   
   sta 5 0           ;; reset accum address

   add.x 4 1         ;; inc vertex address
   lda 4  0          ;; load vertex address
   ldl mdl-end 0     ;; load end address
   jlt 2  0          ;; if greater than model (relative address)
   jmp 8  0          ;; do next vertex

   ;; end: reset current vertex back to start
   ldl mdl-start 0
   sta 4 0

   ;; waggle around the last point a bit
   lda mdl-end 0     ;; load vertex pos
   rnd 0 0           ;; load random vert
   ldl 0.5 0         
   mul 0 0           ;; make it smaller
   add 0 0           ;; add to vertex pos
   sta mdl-end 0     ;; write to model

   jmp 8 0          
   end-check))


(define explode
  (list
   ;; register section
   8 30000 0 ;; control (pc, cycles, stack)
   mdl-size prim-points 1 ;; graphics (size, primtype, renderhints)
   0 0 0 ;; pos
   0 0 0 ;; sensor addr

   ;; program data section
   mdl-start 0 0     ;; 4 address of current vertex
   mdl-start 0 0     ;; 5 address of accum vertex (loop)
   0 0 0             ;; 6 influence
   9999999 0 0             ;; 7 closest

   ;; code section 
   ;; add up differences with every other vertex
   ldi  4  0         ;; load current vertex
   ldi  5  0         ;; load accum vertex
   sub  0  0         ;; get the difference
   
   lda  4 0          ;; are we looking at the current
   lda  5 0          ;; vertex??
   sub  0 0       
   jmz  10 0          ;; skip next section

   ; find the closest vertex
   dup  0  0         ;; dup the difference
   dup  0  0         
   len  0  0         ;; get the length
   lda  7  0         ;; load the closest dist
   len  0  0         ;; get the length
   jlt  3  0         ;; if it's less
   sta  7  0         ;; then store it in closest dist
   jmr  2  0
   drp  0  0         ;; forget it
   
   lda  6  0         ;; load the accumulation
   add  0  0         ;; accumulate
   nrm  0  0         ;; normalise
   sta  6  0         ;; store accumulation
   add.x 5 1         ;; inc accum address

   ;; accumulation iteration 
   lda  5  0         ;; load accum address
   ldl  mdl-end 0    ;; load end address
   jlt  2  0         ;; exit if greater than model end (relative address)
   jmp  8  0         ;; return to accum-loop

   ;; end accum loop
   ;; attact to other verts
;   lda 6  0          ;; load accum
   ldi 4 0
   ldl 0 0
   sub 0 0
   ldl 0.01 0         ;; reverse & make smaller
   mul 0 0

   ;; repel from closest vertex
   lda 7 0
   nrm 0 0
   ldl -0.01 0
   mul 0 0           ;; make smaller
   add 0 0           ;; add to accum result
   ldl 9999 0        ;; reset closest
   sta 7 0

   ;; do the animation
   ldi 4 0           ;; load current vertex
   add 0 0           ;; add the accumulation
   sti 4 0           ;; write to model data

   ;; reset the outer loop
   ldl 0 0           ;; load zero
   sta 6 0           ;; reset accum
   ldl mdl-start 0   
   sta 5 0           ;; reset accum address


   add.x 4 1         ;; inc vertex address
   lda 4  0          ;; load vertex address
   ldl mdl-end 0     ;; load end address
   jlt 2  0          ;; if greater than model (relative address)
   jmp 8  0          ;; do next vertex

   ;; end: reset current vertex back to start
   ldl mdl-start 0
   sta 4 0
 

   jmp 8 0          
   end-check))

(define flock
  '(let ((vertex 512) 
         (accum-vertex 512)  
         (closest 9999)
         (closest-dist 9999)
         (diff 0)
         (vel 1024))
     (loop 1 ;; infinite loop
       (loop (< vertex 532) ;; for every vertex
           
         ;; accumulate over every vertex
         (loop (< accum-vertex 532) 
           (cond 
            ;; if they're not the same vert
            ((not (eq? accum-vertex vertex))
             ;; get vector between the points
             (set! diff (- (read vertex) (read accum-vertex)))
             (cond 
              ;; if it's closer so far
              ((< (mag diff) closest-dist)
               ;; record vector and distance
               (set! closest diff)
               (set! closest-dist (mag closest))))))
           (set! accum-vertex (+ accum-vertex 1)))
         ;; reset accum-vertex for next time
         (set! accum-vertex 512)
         
         ;; calculate velocity as blend with previous
         (write! vel (+ (* (read vel) 0.99)
                        ;; attract to centre
                        (* (+ (* (- (read vertex) (vector 0 0 0)) 0.05)
                              ;; repel from closest vertex
                              (* (normalise closest) -0.15)) 0.01)))
         ;; add velocity to vertex position
         (write! vertex (+ (read vel) (read vertex))) 
         
         ;; reset and increment stuff
         (set! closest-dist 9999)
         (set! vel (+ vel 1))
         (set! vertex (+ vertex 1)))
       ;; reset for main loop
       (set! vertex 512)
       (set! vel 1024))
     ))

(define (terrain-setup)
  (pdata-map! (lambda (t) (rndvec)) "t")
  (pdata-map! (lambda (n) (vmul (vector (crndf) (crndf) 0) 0.001)) "n")
  (pdata-map! (lambda (c) (vector 1 1 1)) "c")

  (translate (vector -1 2 0))
  (rotate (vector -45 0 0))
;  (rotate (vector 0 0 100))
  (scale (vector 2 2 2))
  
  (let ((tsize 1)
        (twidth 8))
    (pdata-index-map! 
     (lambda (i p) 
       (let* ((tpos (modulo i 3))
              (tri (quotient i 3))
              (flip (modulo tri 2))
              (quad (quotient tri 2))
              (col (modulo quad twidth))
              (row (quotient quad twidth)))
         (vadd
          (vector (+ (* row tsize) 10) (* col tsize) 0)
          (vmul
           (if (zero? flip)
               (cond 
                ((eqv? tpos 0) (vector 0 0 0))
                ((eqv? tpos 1) (vector tsize 0 0))
                ((eqv? tpos 2) (vector tsize tsize 0)))
               (cond 
                ((eqv? tpos 0) (vector 0 0 0))
                ((eqv? tpos 1) (vector tsize tsize 0))
                ((eqv? tpos 2) (vector 0 tsize 0))))
           1))))
     "p")))

(define (particles-setup)
  (pdata-map! (lambda (t) (rndvec)) "t")
  (pdata-index-map! (lambda (i c) (vector (/ i 500) (/ i 200) 0.5)) "c")
  (pdata-map! (lambda (n) (vmul (vector (crndf) (crndf) 0) 0.001)) "n")
  
;;  (translate (vector 1.5 5 -5))
  (rotate (vector -45 0 0))
;;  (rotate (vector 0 0 100))
;;  (scale (vector 2 2 2))
  
  (let ((pos (vector 0 0 0)))
    (pdata-index-map! 
     (lambda (i p) 
       (when (zero? (modulo i 3))
             (set! pos (vmul (vsub (rndvec) (vector -5 -1 -1)) 3)))
       (vadd pos (vmul (srndvec) 0.1)))
     "p")))


(define terrain
  '(let ((vertex positions-start)
         (flingdamp (vector -30 -50 0))
         (world (vector 0 0 0)))

     (define recycle 
       (lambda (dir)         
         (write! vertex (+ (*v (read vertex) (vector 1 1 0)) dir))
         (write! (+ vertex 1) (+ (*v (read (+ vertex 1)) (vector 1 1 0)) dir))
         (write! (+ vertex 2) (+ (*v (read (+ vertex 2)) (vector 1 1 0)) dir))

         (let ((a (noise (* (- (read vertex) world) 0.3)))
               (b (noise (* (- (read (+ vertex 1)) world) 0.3)))
               (c (noise (* (- (read (+ vertex 2)) world) 0.3))))

           (write! vertex (+ (read vertex) (+ (*v a (vector 0 0 8)) (vector 0 0 -4))))
           (write! (+ vertex 1) (+ (read (+ vertex 1)) (+ (*v b (vector 0 0 8)) (vector 0 0 -4))))
           (write! (+ vertex 2) (+ (read (+ vertex 2)) (+ (*v c (vector 0 0 8)) (vector 0 0 -4))))

           (define n (normalise (cross (- (read vertex)
                                          (read (+ vertex 2)))
                                       (- (read vertex)
                                          (read (+ vertex 1))))))

           (write! (+ vertex 512) n)
           (write! (+ vertex 513) n)
           (write! (+ vertex 514) n)
           
;           (write! (+ vertex 1024) a)
;           (write! (+ vertex 1025) b)
;           (write! (+ vertex 1026) c)
           )
         ))

     (loop 1
       (set! flingdamp (+ (* flingdamp 0.99)
                          (*v 
                           (swizzle xyz (read reg-fling))
                           (vector 0.01 -0.01 1))))

       (define vel (* flingdamp 0.002))
       (set! world (+ world vel))

       (loop (< vertex positions-end)         

         (write! vertex (+ (read vertex) vel))
         (write! (+ vertex 1) (+ (read (+ vertex 1)) vel))
         (write! (+ vertex 2) (+ (read (+ vertex 2)) vel))

         (cond
          ((> (read vertex) 5)
           (recycle (vector -10 0 0))))

         (cond
          ((< (read vertex) -5)
           (recycle (vector 10 0 0))))

         (cond
          ((> (swizzle yzz (read vertex)) 4)
           (recycle (vector 0 -8 0))))

         (cond
          ((< (swizzle yzz (read vertex)) -4)
           (recycle (vector 0 8 0))))


         (set! vertex (+ vertex 3)))

       (set! vertex positions-start))))

(define testfn
  '(let ((bb 9))
     (define myfun 
       (lambda (a b)
         (trace 9) 
         (* a b)))
     (trace 0)
     (trace (myfun 30 200))
     (trace 1)

     (loop 1)))


(define jelly 0)
(define jelly2 0)

(display "inside jf")(newline)

(define (jelly-setup2)
  (display "hello from nomadic callback")(newline)
  (hint-unlit)

  (with-state
   (hint-unlit)
   (set! jelly (build-jellyfish 512)))
  (with-primitive
   jelly
   (terrain-setup)
   (let ((p (compile-program 10000 prim-triangles 1 terrain)))
     ;;(disassemble p)
     (jelly-compiled p)
     ))

  (with-state
   (hint-unlit)
   (set! jelly2 (build-jellyfish 512)))
  (with-primitive
   jelly2
   (particles-setup)
   ;(let ((p (compile-program 10000 prim-triangles 1 terrain)))
   ;  (jelly-compiled p)
   ;  )
   0)

  (every-frame
   (begin
     (with-primitive 
      jelly 0
      (pdata-set! "x" reg-fling (vector (vx _fling) (vy _fling) 0)))
     (with-primitive 
      jelly2 0
      (pdata-set! "x" reg-fling (vector (vx _fling) (vy _fling) 0)))
     ))
   
  )

(jelly-setup2)