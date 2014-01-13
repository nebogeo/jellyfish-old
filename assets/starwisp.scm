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
                                        ; (hint-unlit)

              (define s (raw-obj spider))

              (with-state
                                        ;   (hint-unlit)
               (set! jelly (build-jellyfish 512)))
              (with-primitive
               jelly
               (terrain-setup)
               (jelly-compiled (compile-program 10000 prim-triangles 1 terrain)))

                                        ;  (with-state
                                        ;   (hint-unlit)
                                        ;   (set! jelly2 (build-jellyfish 512)))
                                        ;  (with-primitive
                                        ;   jelly2
                                        ;   (particles-setup)
                                        ;   (jelly-compiled (compile-program 10000 prim-triangles 1 terrain)))

              (every-frame
               (begin
                 (with-primitive s (rotate (vector 0.5 1 0)))
                 (with-primitive
                  jelly 0
                  (pdata-set! "x" reg-fling (vector (vx _fling) (vy _fling) 0)))
                                        ;(with-primitive
                                        ; jelly2 0
                                        ; (pdata-set! "x" reg-fling (vector (vx _fling) (vy _fling) 0)))
                 ))

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
