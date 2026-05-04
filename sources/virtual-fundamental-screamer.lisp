;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; ML-VIRTUAL-FUNDAMENTAL 
;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ML-VIRTUAL-FUNDAMENTAL uses the screamer software package. Credits are below:

;;; LaHaShem HaAretz U'Mloah

;;; Screamer
;;; A portable efficient implementation of nondeterministic Common Lisp
;;; Version 3.20
;;;
;;; Written by:
;;;
;;;   Jeffrey Mark Siskind (Department of Computer Science, University of Toronto)
;;;   David Allen McAllester (MIT Artificial Intelligence Laboratory)
;;;
;;; Copyright 1991 Massachusetts Institute of Technology. All rights reserved.
;;; Copyright 1992, 1993 University of Pennsylvania. All rights reserved.
;;; Copyright 1993 University of Toronto. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;;; this software and associated documentation files (the "Software"), to deal in
;;; the Software without restriction, including without limitation the rights to
;;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;;; the Software, and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:
;;;
;;; The above copyright and authorship notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Important notice: In this version of Screamer, if Screamer is already
;;; loaded and you wish to recompile the entire file, the recompilation will
;;; proceed much faster if you first do:
;;; (CLRHASH SCREAMER::*FUNCTION-RECORD-TABLE*)

; ================================================================= ;

 (in-package :omlindberg)

(defun make-n-integersv (nbVar) ;adapted from OM-Math-Tools
  (loop for k from 1 to nbVar
        collect (screamer::an-integerv)))

; =================================================================;

(in-package :screamer)

;;; REVISION: 30/04/2026

(defun freq-approx (n)
  (expt 2 (* 1/12 n)))

(defun get-variable-bounds (solution)
 (mapcar #'(lambda (x) 
              (if (variable? x)
                  (list (variable-lower-bound x) (variable-upper-bound x))
                  x))
  solution))
 
(defun virt-fund (nf-list f-min f-max approx)
 (let* ((fund (a-real-betweenv f-min f-max))
        (partials (oml::make-n-integersv (length nf-list)))
        (appr (freq-approx approx)))
  (mapc #'(lambda (freq part)
           (assert! (andv (>=v part (/v (/v freq appr) fund))
                          (<=v part (/v (*v freq appr) fund)))))
        nf-list partials)
  (assert! (apply #'/=v partials))
  (one-value
   (get-variable-bounds
    (solution 
     (list partials fund)
    (reorder #'domain-size
             #'(lambda (x)(< x 1e-6)) 
             #'>
             #'divide-and-conquer-force))))))

(defun well-tempered-virt-fun (chord min-midi max-midi &optional n-common-notes)
 (let ((common-tones (if n-common-notes n-common-notes (length chord)))
       (tempered-chord (om::approx-m chord 2)))
  (all-values 
   (let ((fund (a-member-of (reverse (om::arithm-ser min-midi max-midi 100))))
          intersect)
   (local
    (setf intersect
          (remove-duplicates
           (intersection (om::approx-m (om::f->mc (om::om* (om::mc->f fund)
                                                           '(1 3 5 7 9 11 13 15 17 19 21 27)
                                                           ;(om::arithm-ser 1 50 1)
                                                           ))
                          2)
                         tempered-chord)))
    (if (>= (length intersect) common-tones)
        (cons fund (reverse intersect))
        (fail)))))))

(om::defmethod! ml-virtual-fundamental ((chord list) (approx number) &optional (f-min 100) (f-max 10800))
    :initvals '((6000 6200 6700 7100 7300 7700 8000 8200) 4)
    :indoc '("list-of-midics" "2=semitone,4=quarter-tone,etc." "midics" "midics" ) 
    :icon 01
    :menuins '( (1 (("2" 2) ("4" 4) ("8" 8) ("16" 16)) ) )
    :numouts 2
    :doc "Returns the highest virtual fundamental of <chord-midics> with approximations <approx>. 
         Optional:  from f-min <note-midics> to f-max <note-midics>. 
         First output in midicents.
         Second output is a list with partial numbers and frequencies (min. and max.)."
         
 (setf f-max (first chord))
 
 (let* ((approximation (/ 2 approx))
 
        (solution (virt-fund (om::mc->f chord) (om::mc->f f-min) (om::mc->f f-max) approximation)))
  (values (om::remove-dup
           (mapcar #'(lambda (fund)
                      (om::f->mc (om::om* (first solution) fund)))
            (second solution))
           'equal 1)
           solution)))


