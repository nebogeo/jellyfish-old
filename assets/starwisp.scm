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

(define instructions (list "nop" "jmp" "jmz" "jlt" "jgt" "ldl" "lda" "ldi" "sta"
                           "sti" "add" "sub" "mul" "div" "abs" "sin" "atn" "dot"
                           "crs" "sqr" "len" "dup" "drp" "cmp" "shf" "bld" "ret"
                           "dbg" "nrm" "add.x" "add.y" "add.z"))

(define (make-instr-but instr)
  (button (make-id (string-append instr "-button")) instr 18 fillwrap
          (lambda () (list (finish-activity 99)))))

(define (make-instr-row l)
  (apply
   horiz
   (map
    (lambda (i)
      (make-instr-but (list-ref instructions i)))
    l)))

(define (op-or-num->string v)
  (if (> v 31) (number->string v)
      (list-ref instructions v)))

(define (make-instr v)
  (display v)(newline)
  (horiz
;;  (toggle-button (make-id "exe") "exe" 10 (layout 'wrap-content 'fill-parent 0.2 'left)
;;                  (lambda (v) (list)))
   (button (make-id "inst")  (op-or-num->string (vx v)) 20 (layout 100 25 1 'left 0)
           (lambda () (list (start-activity "instr-buttons" 0 ""))))
   (button (make-id "b1") (number->string (vy v)) 20 (layout 100 25 1 'left 0)
           (lambda () (list (start-activity "float-value" 2 ""))))
   (button (make-id "b2") (number->string (vz v)) 20 (layout 100 25 1 'left 0)
           (lambda () (list (start-activity "float-value" 2 ""))))))

(define (jelly->instrs jelly start end)
  (define (_ n)
    (cond
     ((eqv? n end) '())
     (else (cons
            (make-instr (pdata-ref "x" n))
            (_ (+ n 1))))))
  (with-primitive
   jelly
   (_ start)))

(define (update-livecode jelly)
  (list
   (update-widget 'linear-layout (get-id "livecode") 'contents 
                  (list
                   (apply vert (jelly->instrs jelly 0 19))
                   (apply vert (jelly->instrs jelly 20 39))))))

(display "hello")(newline)

(define-fragment-list '())

(define-activity-list
  (activity
   "main"
   (frame-layout
    (make-id "fl")
    (layout 'fill-parent 'fill-parent 1 'left 0)

    (list
     (nomadic (make-id "b2x") (layout 1024 524 1 'left 0) 
              (lambda ()
                (display "hello from nomadic callback")(newline)
                (clear)
 ; (hint-unlit)

  (with-state
;   (hint-unlit)
   (set! jelly (build-jellyfish 512)))
  (with-primitive
   jelly
   (terrain-setup)
   (jelly-compiled (compile-program 100 prim-triangles 1 synthtest)))

;  (with-state
;   (hint-unlit)
;   (set! jelly2 (build-jellyfish 512)))
;  (with-primitive
;   jelly2
;   (particles-setup)
;   (jelly-compiled (compile-program 10000 prim-triangles 1 terrain)))

  (every-frame
   (begin
     (with-primitive 
      jelly 0
      (pdata-set! "x" reg-fling (vector (vx _fling) (vy _fling) 0)))
     ;(with-primitive 
     ; jelly2 0
     ; (pdata-set! "x" reg-fling (vector (vx _fling) (vy _fling) 0)))
     ))
   
     ))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "float-value"
   (vert
    (text-view (make-id "number") "202.133" 60 fillwrap)
    (text-view (make-id "100s-text") "100s" 20 fillwrap)
    (seek-bar (make-id "100s") 100 fillwrap (lambda (v) '()))
    (text-view (make-id "10s-text") "10s" 20 fillwrap)
    (seek-bar (make-id "10s") 100 fillwrap (lambda (v) '()))
    (text-view (make-id "10s-text") "1s" 20 fillwrap)
    (seek-bar (make-id "1s") 100 fillwrap (lambda (v) '()))
    (button (make-id "done") "Done" 20 fillwrap
            (lambda () (list (finish-activity 99)))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "instr-buttons"
   (vert
    (make-instr-row (list 0  1  2  3  4 ))
    (make-instr-row (list 5  6  7  8  9 ))
    (make-instr-row (list 10 11 12 13 14 ))
    (make-instr-row (list 15 16 17 18 19 ))
    (make-instr-row (list 20 21 22 23 24 ))
    (make-instr-row (list 25 26 27 28 29))
    (make-instr-row (list 30 31))
    )
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