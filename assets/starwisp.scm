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

(define instructions (list "nop" "jmp" "jmz" "jlt" "jgt" "ldl" "lda" "ldi" "sta"
                           "sti" "add" "sub" "mul" "div" "abs" "sin" "atn" "dot"
                           "crs" "sqr" "len" "dup" "drp" "cmp" "shf" "bld" "ret"
                           "dbg" "nrm" "add.x" "add.y" "add.z"))

(define (make-instr)
  (horiz
   (toggle-button (make-id "exe") "exe" 10 (layout 'wrap-content 'fill-parent 0.2 'left) (lambda (v) (list)))
   (spinner (make-id "inst") instructions fillwrap (lambda (v) (list)))
   (button (make-id "b1") "0.1" 20 fillwrap
           (lambda () (list (start-activity "main" 2 ""))))
   (button (make-id "b2") "100" 20 fillwrap
           (lambda () (list (start-activity "main" 2 ""))))))


(define-activity-list
  (activity
   "splash"
   (frame-layout
    (make-id "fl")
    (layout 'fill-parent 'fill-parent 1 'left)

    (list
     (button (make-id "b2x") "TEST" 50 fillwrap (lambda () (list)))
     (scroll-view
      (make-id "scroller")
      (layout 'wrap-content 'wrap-content 1 'left)
      (list
       (linear-layout
        (make-id "fl")
        'horizontal
        (layout 1000 'wrap-content 1 'left)
        (list
         (vert
          (make-instr)
          (make-instr)
          (make-instr)
          (make-instr)
          (make-instr)
          (make-instr)
          (make-instr)
          (make-instr)
          (make-instr)
          (make-instr)
          (make-instr)
          (make-instr)
          (make-instr)
          (make-instr))
         (vert
          (make-instr)
          (make-instr)
          (make-instr)
          (make-instr)
          (make-instr)
          (make-instr)
          (make-instr)
          (make-instr)
          (make-instr)
          (make-instr)
          (make-instr)
          (make-instr)
          (make-instr)
          (make-instr)))
        )))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "main"
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



;  (activity
;   "splash"
;   (vert
;    (text-view (make-id "splash-title") "Mongoose 2000" 40 fillwrap)
;    (text-view (make-id "splash-about") "Advanced mongoose technology" 20 fillwrap)
;    (spacer 20)
;    (button (make-id "f2") "Get started!" 20 fillwrap
;            (lambda () (list (start-activity-goto "main" 2 "")))))
;   (lambda (activity arg)
;     (activity-layout activity))
;   (lambda (activity arg) '())
;   (lambda (activity) '())
;   (lambda (activity) '())
;   (lambda (activity) '())
;   (lambda (activity) '())
;   (lambda (activity requestcode resultcode) '()))


;  (activity
;   "main"
;   (vert
;    (text-view (make-id "title") "Mongoose 2000" 40 fillwrap)
;    (spacer 10)
;    (button (make-id "main-sync") "Sync Data" 20 fillwrap (lambda () (list)))
;    (button (make-id "main-sync") "Experiment" 20 fillwrap (lambda () (list)))
;    (button (make-id "main-sync") "Manage Packs" 20 fillwrap (lambda () (list)))
;    (button (make-id "main-sync") "Tag Location" 20 fillwrap (lambda () (list)))
;    (button (make-id "main-sync") "Send Database" 20 fillwrap (lambda () (list)))
;    )
;   (lambda (activity arg)
;     (activity-layout activity))
;   (lambda (activity arg) (list))
;   (lambda (activity) '())
;   (lambda (activity) '())
;   (lambda (activity) '())
;   (lambda (activity) '())
;   (lambda (activity requestcode resultcode)
;     (list
;      (update-widget 'linear-layout (get-id "main-field-list") 'contents
;                     (build-field-buttons)))))



  )
