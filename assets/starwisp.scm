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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define nop 0) (define jmp 1) (define jmz 2) (define jlt 3) (define jgt 4)
(define ldl 5) (define lda 6) (define ldi 7) (define sta 8) (define sti 9)
(define add 10) (define sub 11) (define mul 12) (define div 13) (define abs 14)
(define _sin 15) (define atn 16) (define dot 17) (define crs 18) (define sqr 19)
(define len 20) (define dup 21) (define drp 22) (define cmp 23) (define shf 24)
(define bld 25) (define ret 26) (define dbg 27) (define nrm 28)
(define add.x 29) (define add.y 30) (define add.z 31) (define end-check 999)

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
   (button (make-id "inst")  (op-or-num->string (vx v)) 20 (layout 100 25 1 'left)
           (lambda () (list (start-activity "instr-buttons" 0 ""))))
   (button (make-id "b1") (number->string (vy v)) 20 (layout 100 25 1 'left)
           (lambda () (list (start-activity "float-value" 2 ""))))
   (button (make-id "b2") (number->string (vz v)) 20 (layout 100 25 1 'left)
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

(define jelly 0)

(define-activity-list
  (activity
   "main"
   (frame-layout
    (make-id "fl")
    (layout 'fill-parent 'fill-parent 1 'left)

    (list
     (nomadic (make-id "b2x") (layout 512 1024 1 'left)
              (lambda ()
                (display "hello from nomadic callback")(newline)
                (with-state
                 (hint-unlit)
                 (set! jelly (build-jellyfish 512)))
                (with-primitive
                 jelly

                 (pdata-index-map!
                  (lambda (i c)
                    (let ((ir (modulo (* i 4) (pdata-size)))
                          (ig (modulo (* i 20) (pdata-size)))
                          (ib (modulo (* i 3) (pdata-size))))
                      (vmul (vector (/ ir (pdata-size))
                                    (/ ig (pdata-size))
                                    (/ ib (pdata-size))) 0.5)))
                  "c")

                 (jelly-prog v2-test))))
     (scroll-view
      (make-id "scroller")
      (layout 'wrap-content 'wrap-content 1 'left)
      (list
       (linear-layout
        (make-id "livecode")
        'horizontal
        (layout 2000 1000 1 'left)
        (list
         (button (make-id "update") "Update" 20 fillwrap (lambda () (update-livecode jelly))))
        )))
     ))
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
