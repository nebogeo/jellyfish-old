;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(display "jellyfish.scm")(newline)
(define spider "")

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

(define (obj-setup)
  (pdata-map! (lambda (p) (srndvec)) "p")
  (pdata-map! (lambda (c) (rndvec)) "c")
  (hint-unlit))


(define obj-test
  '(let ((t 0))
     (define rotx
       (lambda (a)
         (write! reg-tx-rotateb (swizzle zyx (*v (sincos a) (vector 1 -1 0))))
         (write! reg-tx-rotatec (swizzle zxy (sincos a)))))

     (define roty
       (lambda (a)
         (write! reg-tx-rotatea (swizzle yzx (*v (sincos a) (vector 1 -1 0))))
         (write! reg-tx-rotatec (swizzle xzy (sincos a)))))

     (define rotz
       (lambda (a)
         (write! reg-tx-rotatea (swizzle yxz (*v (sincos a) (vector 1 -1 0))))
         (write! reg-tx-rotateb (swizzle xyz (sincos a)))))

     (define move-along-y
       (lambda (speed)
         (write! reg-tx-translate
                 (+ (read reg-tx-translate)
                    (* (read reg-tx-rotatea) speed)))))

     (loop 1
           (roty t)
           (move-along-y 0.03)
           (ignore (set! t (+ t 1)))
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

(define (print-obj c)
  (cond
   ((< c (pdata-size))
    (msg (pdata-ref "p" c))
    (print-obj (+ c 1)))))

(define (jelly-setup2)
  (display "hello from nomadic callback")(newline)

  (define s (raw-obj spider))
  (msg s)
  (with-primitive
   s
   (hint-wire)
   (msg (pdata-ref "n" 10)))

  (every-frame (with-primitive s (rotate (vector 0.5 1 0))))

;  (build-cube)
  ;(with-state
  ; (hint-wire)
  ; (hint-unlit)
  ; (set! jelly (build-jellyfish 512)))
  ;(with-primitive
  ; jelly
  ; (obj-setup)
  ; (let ((p (compile-program 100 prim-triangles 1 obj-test)))
  ;   (disassemble p)
  ;   (jelly-compiled p)
  ;   ))

 ; (every-frame
  ; (begin
  ;   (with-primitive
  ;    jelly 0
  ;    (pdata-set! "x" reg-fling (vector (vx _fling) (vy _fling) 0)))
  ;   ))
  )



(set! spider "v 0.038612 -0.728093 -0.016661
v 0.762212 -0.175308 0.509059
v -0.237772 -0.175308 0.833979
v -0.855813 -0.175308 -0.016661
v -0.237772 -0.175308 -0.867301
v 0.762212 -0.175308 -0.542381
v 0.314997 0.719121 0.833979
v -0.684988 0.719121 0.509059
v -0.684988 0.719121 -0.542381
v 0.314997 0.719121 -0.867301
v 0.933037 0.719121 -0.016661
v 0.038612 1.271906 -0.016661
v 0.647048 0.521884 0.606315
v 0.819118 0.521884 0.369485
v 0.771558 0.272863 0.515852
v 2.511734 0.921449 1.599268
v 2.464175 0.672428 1.745636
v 2.339664 0.921449 1.836098
v 4.240845 -0.658087 2.855567
v 4.068775 -0.658087 3.092397
v 4.193285 -0.907107 3.001935
v -0.388091 0.535627 -0.784308
v -0.067394 0.535627 -0.888511
v -0.244668 0.248781 -0.888511
v -1.366142 1.140074 -3.794393
v -1.222719 0.853229 -3.898595
v -1.045445 1.140074 -3.898595
v -2.173658 -0.466432 -6.279633
v -1.852961 -0.466432 -6.383835
v -2.030235 -0.753277 -6.383835
v -0.875408 0.551986 0.162847
v -0.875408 0.551986 -0.196170
v -0.933737 0.246580 -0.016661
v -5.142687 1.366984 0.162847
v -5.201015 1.061578 -0.016661
v -5.142687 1.366984 -0.196170
v -7.177661 -1.053155 -0.196170
v -7.177661 -1.053155 0.162848
v -7.235990 -1.358560 -0.016661
v -0.114001 0.528707 0.900290
v -0.376895 0.528707 0.814870
v -0.259323 0.293563 0.900290
v -1.249704 1.230586 4.395571
v -1.395026 0.995442 4.395571
v -1.512598 1.230586 4.310151
v -2.374467 -1.335157 7.857181
v -2.637361 -1.335157 7.771760
v -2.519789 -1.570300 7.857181
v 0.669222 0.545155 -0.686431
v 0.870474 0.545155 -0.409438
v 0.814848 0.253903 -0.580627
v 3.165960 1.087031 -2.077247
v 3.110334 0.795778 -2.248436
v 2.964708 1.087031 -2.354240
v 6.069157 -0.980113 -4.186595
v 5.867905 -0.980113 -4.463588
v 6.013531 -1.271365 -4.357785
vn 0.187597 -0.794651 0.577354
vn 0.607065 -0.794652 0.000000
vn -0.491122 -0.794652 0.356829
vn -0.491122 -0.794652 -0.356829
vn 0.187596 -0.794651 -0.577354
vn 0.982246 -0.187597 0.000000
vn 0.303536 -0.187589 0.934172
vn -0.794649 -0.187587 0.577359
vn -0.794649 -0.187587 -0.577359
vn 0.303535 -0.187589 -0.934172
vn 0.491122 0.794652 0.356829
vn -0.187597 0.794651 0.577353
vn -0.607065 0.794652 0.000000
vn -0.187596 0.794651 -0.577354
vn 0.491122 0.794652 -0.356829
vn 0.794649 0.187588 0.577359
vn -0.303535 0.187589 -0.934172
vn -0.982246 0.187597 -0.000000
vn -0.303535 0.187589 0.934172
vn 0.794649 0.187586 -0.577360
vn 0.222713 -0.525260 0.821280
vn -0.712272 -0.525267 -0.465578
vn -0.712272 -0.525267 -0.465577
vn 0.403813 0.866519 -0.293394
vn -0.151759 0.982248 0.110262
vn -0.433165 -0.491126 -0.755754
vn 0.584926 -0.491118 0.645496
vn 0.986515 -0.129858 -0.099621
vn 0.986515 -0.129861 -0.099619
vn 0.399576 -0.129863 -0.907455
vn 0.399575 -0.129865 -0.907455
vn 0.561808 0.719552 -0.408187
vn 0.561807 0.719553 -0.408187
vn 0.845254 -0.522378 -0.112542
vn -0.617656 -0.522389 -0.587886
vn -0.178045 0.817338 0.547959
vn 0.057969 0.982248 -0.178407
vn -0.852622 -0.491124 -0.178417
vn 0.794661 -0.491111 0.356824
vn 0.206622 -0.127695 0.970052
vn -0.737347 -0.127704 0.663333
vn -0.737347 -0.127705 0.663333
vn -0.215536 0.716605 0.663341
vn -0.215536 0.716604 0.663342
vn 0.584259 -0.491275 0.645980
vn 0.584259 -0.491275 -0.645980
vn -0.765384 0.643574 -0.000000
vn 0.187597 0.982246 0.000000
vn -0.093797 -0.491114 -0.866031
vn -0.093797 -0.491114 0.866031
vn -0.832563 -0.153777 0.532157
vn -0.832563 -0.153778 0.532157
vn -0.832563 -0.153777 -0.532157
vn -0.832563 -0.153778 -0.532157
vn -0.659660 0.751564 0.000000
vn -0.649589 -0.524675 0.550227
vn 0.848952 -0.524664 0.063307
vn -0.161842 0.851888 -0.498090
vn 0.057969 0.982248 0.178407
vn 0.794662 -0.491111 -0.356824
vn 0.794661 -0.491111 -0.356824
vn -0.852622 -0.491124 0.178417
vn -0.852622 -0.491124 0.178416
vn -0.648801 -0.048530 -0.759409
vn 0.078513 -0.048522 -0.995732
vn -0.246753 0.601997 -0.759416
vn 0.136034 -0.521195 -0.842526
vn -0.759261 -0.521202 0.389708
vn 0.480826 0.804215 0.349348
vn -0.151761 0.982248 -0.110263
vn -0.433163 -0.491126 0.755755
vn -0.433162 -0.491127 0.755755
vn 0.584925 -0.491118 -0.645496
vn 0.962136 -0.024179 0.271495
vn 0.962136 -0.024178 0.271496
vn 0.555506 -0.024183 0.831161
vn 0.555505 -0.024184 0.831161
vn 0.667902 0.564287 0.485270
vn 0.667903 0.564286 0.485270
usemtl (null)
s off
f 3//1 1//1 2//1
f 2//2 1//2 6//2
f 4//3 1//3 3//3
f 5//4 1//4 4//4
f 6//5 1//5 5//5
f 2//6 6//6 11//6
f 3//7 2//7 7//7
f 4//8 3//8 8//8
f 5//9 4//9 9//9
f 6//10 5//10 10//10
f 7//11 11//11 12//11
f 8//12 7//12 12//12
f 9//13 8//13 12//13
f 10//14 9//14 12//14
f 11//15 10//15 12//15
f 19//16 20//16 21//16
f 28//17 29//17 30//17
f 38//18 37//18 39//18
f 46//19 47//19 48//19
f 56//20 55//20 57//20
f 52//21 53//21 55//21
f 53//21 57//21 55//21
f 53//22 54//22 56//22
f 53//23 56//23 57//23
f 54//24 52//24 56//24
f 52//24 55//24 56//24
f 49//25 50//25 52//25
f 49//25 52//25 54//25
f 51//26 49//26 54//26
f 51//26 54//26 53//26
f 50//27 51//27 53//27
f 50//27 53//27 52//27
f 11//28 6//28 51//28
f 11//29 51//29 50//29
f 6//30 10//30 51//30
f 10//31 49//31 51//31
f 10//32 11//32 49//32
f 11//33 50//33 49//33
f 44//34 43//34 46//34
f 44//34 46//34 48//34
f 45//35 44//35 47//35
f 44//35 48//35 47//35
f 43//36 45//36 47//36
f 43//36 47//36 46//36
f 40//37 41//37 45//37
f 40//37 45//37 43//37
f 41//38 42//38 44//38
f 41//38 44//38 45//38
f 42//39 40//39 43//39
f 42//39 43//39 44//39
f 3//40 7//40 42//40
f 7//40 40//40 42//40
f 8//41 3//41 42//41
f 8//42 42//42 41//42
f 7//43 8//43 41//43
f 7//44 41//44 40//44
f 35//45 34//45 38//45
f 35//45 38//45 39//45
f 36//46 35//46 37//46
f 35//46 39//46 37//46
f 34//47 36//47 37//47
f 34//47 37//47 38//47
f 31//48 32//48 36//48
f 31//48 36//48 34//48
f 32//49 33//49 35//49
f 32//49 35//49 36//49
f 33//50 31//50 34//50
f 33//50 34//50 35//50
f 4//51 8//51 33//51
f 8//52 31//52 33//52
f 9//53 4//53 33//53
f 9//54 33//54 32//54
f 8//55 9//55 32//55
f 8//55 32//55 31//55
f 26//56 25//56 28//56
f 26//56 28//56 30//56
f 27//57 26//57 29//57
f 26//57 30//57 29//57
f 25//58 27//58 29//58
f 25//58 29//58 28//58
f 22//59 23//59 27//59
f 22//59 27//59 25//59
f 23//60 24//60 26//60
f 23//61 26//61 27//61
f 24//62 22//62 25//62
f 24//63 25//63 26//63
f 5//64 9//64 24//64
f 9//64 22//64 24//64
f 10//65 5//65 24//65
f 10//65 24//65 23//65
f 9//66 10//66 22//66
f 10//66 23//66 22//66
f 17//67 16//67 19//67
f 17//67 19//67 21//67
f 18//68 17//68 20//68
f 17//68 21//68 20//68
f 16//69 18//69 20//69
f 16//69 20//69 19//69
f 14//70 13//70 18//70
f 14//70 18//70 16//70
f 13//71 15//71 17//71
f 13//72 17//72 18//72
f 15//73 14//73 16//73
f 15//73 16//73 17//73
f 2//74 11//74 15//74
f 11//75 14//75 15//75
f 7//76 2//76 15//76
f 7//77 15//77 13//77
f 11//78 7//78 13//78
f 11//79 13//79 14//79")


;;(jelly-setup2)
