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

(defun make-n-integersv (nbVar);adapted from OM-Math-Tools
  (loop for k from 1 to nbVar
        collect (s::an-integerv)))

; =================================================================;

(in-package :s)

(defun approxv (n)
 (funcallv #'expt 2 (*v 1/12 n)))
  
(defun virt-fund (nf-list f-min f-max approx) ;04/21/2023

  (setf appr (approxv approx))

     (let* ((fund (a-real-betweenv f-min f-max))
              (VarArray (oml::make-n-integersv (length nf-list))))

              (local (dotimes (x (length nf-list))
                        (assert!  (andv (>=v (nth x VarArray) (/v (/v (nth x nf-list) appr) fund) )
                                                (<=v (nth x VarArray) (/v (*v (nth x nf-list) appr) fund) ) ))))

              (eval `(assert! (/=v ,. VarArray)))

   (one-value
     (solution 
      VarArray 
     (reorder #'domain-size
          #'(lambda (x) (declare (ignore x)))
          #'>
          #'linear-force)))))

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
				
 (let* ((fundamentals (om::om/ (om::mc->f chord) solution))
         (results (om::x-append
                  (list solution)         
                  (/  (om::list-max fundamentals) (expt 2 (* 1/12 approximation)))
                  (* (expt 2 (* 1/12 approximation)) (om::list-min fundamentals)))))
					  
  (values (om::remove-dup 
	       (om::f->mc 
			(list (om::x-append (second results) (om::om* (first results) (second results)))
                  (om::x-append (third results) (om::om* (first results) (third results)))))
          'equal 1)
          (write results)))))
