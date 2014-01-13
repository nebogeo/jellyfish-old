;; Starwisp Copyright (C) 2013 Dave Griffiths
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(display "starwisp.scm")(newline)

(define-fragment-list '())

(define-activity-list
  (activity
   "main"
   (nomadic (make-id "b2x") (layout 'fill-parent 'fill-parent 1 'left 0)
            (lambda ()
              (display "hello from nomadic callback")(newline)
              (clear)



  (set! jelly (build-jellyfish 512))
  (set! jelly2 (build-jellyfish 512))

;  (with-primitive
;   jelly
;   (terrain-setup)
;   (jelly-compiled (compile-program 10000 prim-triangles 1 terrain)))

  (define s1 (raw-obj (list-ref spider 0)))
  (define s2 (raw-obj (list-ref spider 1)))
  (define s3 (raw-obj (list-ref spider 2)))

  (msg s1 s2 s3)

  (with-primitive
   jelly2
   (scale (vector 0.5 0.5 0.5))
   (pdata-index-map! (lambda (i p) (with-primitive s2 (pdata-ref "p" i))) "p")
   (pdata-index-map! (lambda (i p) (with-primitive s2 (pdata-ref "p" i))) "t")
   (pdata-index-map! (lambda (i p) (with-primitive s3 (pdata-ref "p" i))) "c")

   (pdata-index-map! (lambda (i n) (with-primitive s1 (pdata-ref "n" i))) "n")
;;   (pdata-index-map! (lambda (i n) (with-primitive s2 (pdata-ref "n" i))) "n2")
;;   (pdata-index-map! (lambda (i n) (with-primitive s3 (pdata-ref "n" i))) "n3")

   (let ((p (compile-program 10000 prim-triangles 1 obj-test)))
     (disassemble p)
     (jelly-compiled p)
     ))


  (destroy s1)
  (destroy s2)
  (destroy s3)

  ;(every-frame
  ; (begin
  ;   (with-primitive
  ;    jelly 0
  ;    (pdata-set! "x" reg-fling (vector (vx _fling) (vy _fling) 0)))
  ;                                      ;(with-primitive
  ;                                      ; jelly2 0
  ;                                      ; (pdata-set! "x" reg-fling (vector (vx _fling) (vy _fling) 0)))
  ;   ))













              ))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  )

(msg "done....")
