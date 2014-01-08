;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(display "jellyfish.scm")(newline)

(define (jelly-compiled code)
  (define addr 0)
  (for-each
   (lambda (v)
     (pdata-set! "x" addr v)
     (set! addr (+ addr 1)))
   code))

(define (terrain-setup)
  (pdata-map! (lambda (n) (vmul (vector (crndf) (crndf) 0) 0.001)) "n")
  (pdata-map! (lambda (c) (vector 1 1 1)) "c")

  (texture 1)

  (translate (vector -1 2 0))
  (rotate (vector -45 0 0))
;  (rotate (vector 0 0 100))
  (scale (vector 1.5 1.5 1.5))
  
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
     "p"))

  (pdata-map! (lambda (t) (vector 0 0 0)) "t")
  )

(define synthtest
  '(let ((delay 99999))
     (loop 1

           (play-now 
            (mul (adsr 0 0.1 0 2) 
                 (sine 3000)))
           
           (loop (> delay 0)
                 (-- delay))
           (set! delay 99999)
           )))

(define terrain
  '(let ((vertex positions-start)
         (flingdamp (vector -5 10 0))
         (world (vector 0 0 0)))

     ;; recycle a triangle which is off the screen
     (define recycle 
       (lambda (dir)         
         ;; shift along x and y coordinates:
         ;; set z to zero for each vertex
         (write! vertex       
                 (+ (*v (read vertex) 
                        (vector 1 1 0)) dir))
         (write! (+ vertex 1) 
                 (+ (*v (read (+ vertex 1)) 
                        (vector 1 1 0)) dir))
         (write! (+ vertex 2) 
                 (+ (*v (read (+ vertex 2)) 
                        (vector 1 1 0)) dir))

         ;; get the perlin noise values for each vertex
         (let ((a (noise (* (- (read vertex) world) 0.2)))
               (b (noise (* (- (read (+ vertex 1)) 
                               world) 0.2)))
               (c (noise (* (- (read (+ vertex 2))
                               world) 0.2))))

           ;; set the z coordinate for height
           (write! vertex 
                   (+ (read vertex) 
                      (+ (*v a (vector 0 0 8)) 
                         (vector 0 0 -4))))
           (write! (+ vertex 1) 
                   (+ (read (+ vertex 1)) 
                      (+ (*v b (vector 0 0 8)) 
                         (vector 0 0 -4))))
           (write! (+ vertex 2) 
                   (+ (read (+ vertex 2)) 
                      (+ (*v c (vector 0 0 8)) 
                         (vector 0 0 -4))))

           ;; recalculate normals
           (define n (normalise 
                      (cross (- (read vertex)
                                (read (+ vertex 2)))
                             (- (read vertex)
                                (read (+ vertex 1))))))

           ;; write to normal data
           (write! (+ vertex 512) n)
           (write! (+ vertex 513) n)
           (write! (+ vertex 514) n)

           ;; write the z height as texture coordinates
           (write! (+ vertex 1536) 
                   (*v (swizzle zzz a) (vector 0 0.1 0)))          
           (write! (+ vertex 1537) 
                   (*v (swizzle zzz b) (vector 0 0.1 0)))          
           (write! (+ vertex 1538) 
                   (*v (swizzle zzz c) (vector 0 0.1 0))))))

     ;; forever
     (loop 1
       ;; add inertia to the fling/gamepad joystick input
       (set! flingdamp (+ (* flingdamp 0.99)
                          (*v
                           (read reg-fling)
                           (vector 0.01 -0.01 0))))

       (define vel (* flingdamp 0.002))
       ;; update the world coordinates
       (set! world (+ world vel))

       ;; for each vertex
       (loop (< vertex positions-end)         

         ;; update the vertex position
         (write! vertex (+ (read vertex) vel))
         (write! (+ vertex 1) (+ (read (+ vertex 1)) vel))
         (write! (+ vertex 2) (+ (read (+ vertex 2)) vel))

         ;; check for out of area polygons to recycle 
         (cond
          ((> (read vertex) 5.0)
           (recycle (vector -10 0 0)))         
          ((< (read vertex) -5.0)
           (recycle (vector 10 0 0))))
         
         (cond
          ((> (swizzle yzz (read vertex)) 4.0)
           (recycle (vector 0 -8 0)))
          ((< (swizzle yzz (read vertex)) -4.0)
           (recycle (vector 0 8 0))))

         (set! vertex (+ vertex 3)))
       (set! vertex positions-start))))


(define jelly 0)

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

  (every-frame
   (begin
     (with-primitive 
      jelly 0
      (pdata-set! "x" reg-fling (vector (vx _fling) (vy _fling) 0)))
     ))
  )

(jelly-setup2)