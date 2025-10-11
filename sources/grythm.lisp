(in-package :omlindberg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GRYTHM FROM PWGL by Magnus Lindberg ;;;
;;;                                     ;;;
;;; Ported to PWGL by Mikael Laurson    ;;;
;;;                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; IN PROGRESS...

;;;;;;;;;;;;
;; GESTURE 
;; TODO: COMPLEX - LS - SL - GRACE 1,2 and 3

(defun g-rit (size) 
(om::arithm-ser 1 size 1))

(defun g-acc (size) 
(reverse (om::arithm-ser 1 size 1)))

(defun g-pulse (size)
 (om::repeat-n 1 size))

(defun g-zigzag1  (size) 
(let ((numbers (om::arithm-ser 1 size 1)))
 (remove nil 
  (om::flat (loop for na in numbers
         for nd in (reverse numbers)
         collect (cond ((< na nd) (list na nd))
                              ((= na nd) na)
                              ((> na nd) nil)))))))

(defun g-zigzag2 (size) 
(let* ((numbers (om::arithm-ser 1 size 1))
       (partial-result (loop for na in numbers
         for nd in (reverse numbers)
         collect (cond ((< na nd) (list na nd))
                              ((= na nd) na)
                              ((> na nd) nil)
                              (t nil)))))
 (reverse 
 (remove nil
  (om::flat (loop for elem in partial-result
  collect (cond ((listp elem) (reverse elem))
                       ((numberp elem) elem)
                        (t nil))))))))

(defun g-zigzag3 (size) 
(let* ((middle (ceiling (/ size 2)))
         (numbers1 (om::arithm-ser 1 middle 1))
         (numbers2 (om::arithm-ser (1+ middle) size 1)))
 (remove nil
  (om::flat 
   (om::mat-trans (list numbers1 numbers2))))))

(defun g-zigzag4 (size) 
(let* ((middle (floor (/ size 2)))
         (numbers1 (om::arithm-ser 1 middle 1))
         (numbers2 (om::arithm-ser (1+ middle) size 1)))
(remove nil
  (om::flat 
   (om::mat-trans (list (reverse numbers1) (reverse numbers2)))))))

(defun g-melody (size) 
(let* ((odd (om::arithm-ser 1 size 2))
         (even (om::arithm-ser 2 size 2)))
(om::x-append (reverse odd) even)))

(om::defmethod! gesture ((name string) (size integer))
       :initvals '("rit" 8)
       :indoc '("string" "integer") 
       :icon 01
       :menuins '( (0 (("rit" "rit") ("acc" "acc") ("pulse" "pulse") ("zigzag1" "zigzag1")
                       ("zigzag2" "zigzag2") ("zigzag3" "zigzag3") ("zigzag4" "zigzag4")
                       ("melody" "melody"))))
       :doc "returns a grtm. The gesture type depends on name (the first
input is a menuin containing a library of gesture names).
Example results with different names (size = 7):
 rit      -> (1 2 3 4 5 6 7) 
 acc      -> (7 6 5 4 3 2 1) 
 pulse    -> (1 1 1 1 1 1 1) 
 zig-zag1 -> (1 7 2 6 3 5 4) 
 zig-zag2 -> (4 3 5 2 6 1 7)
 zig-zag3 -> (1 5 2 6 3 7 4) 
 zig-zag4 -> (3 7 2 6 1 5 4) 
 melody   -> (7 5 3 1 2 4 6)."
 ;ls       -> (28 4 1 0 2 3 5) 
 ;sl       -> (4 1 0 2 3 5 28) 
 ;complex  -> (4 1 6 0 2 3 5)
 ;grace1   -> (1 1 1 1 1 1 28) 
 ;grace2   -> (1 1 1 11 1 1 10) 
 ;grace3   -> (1 1 1 20 1 1 1 12 1 1 1 34) ;; note here size = 12!

(cond  
 ((equal name "rit") (g-rit size))
 ((equal name "acc") (g-acc size))
 ((equal name "pulse") (g-pulse size))
 ((equal name "zigzag1") (g-zigzag1 size))
 ((equal name "zigzag2") (g-zigzag2 size))
 ((equal name "zigzag3") (g-zigzag3 size))
 ((equal name "zigzag4") (g-zigzag4 size)) 
 ((equal name "melody") (g-melody size))
 (t nil)))

;;;;;;;;;
;; SIZE

(om::defmethod! gtempo ((ratios list) (t1 number) (t2 number))
       :initvals '((1/16 1/16 1/16 1/16 1/8 1/8 1/4 1/4) 60 90)
       :indoc '("list" "number" "number") 
       :icon 01
       :doc "Scales  grtm with the ratio t1/t2."
 (let ((proportion (/ t1 t2)))
 (om::om* ratios proportion)))

(om::defmethod! gpercentage ((ratios list) (percent number))
       :initvals '((1/16 1/16 1/16 1/16 1/8 1/8 1/4 1/4) 80)
       :indoc '("list" "number") 
       :icon 01
       :doc "Scales the durations of grtm  with the ratio  percentage/100."
 (om::om* ratios (/ percent 100)))

(om::defmethod! gsize ((ratios list) (size number))
       :initvals '((1/16 1/16 1/16 1/16 1/8 1/8 1/4 1/4) 3/2)
       :indoc '("list" "number") 
       :icon 01
       :doc "scales the durations of grtm so that the sum of the resulting
durations is equal to size."
 (let ((rests-pos (remove nil (loop for n in ratios
                                  for x from 0 to (1- (length ratios))
                          collect (if (minusp n) x nil))))
        (new-size (om::om-scale/sum (om::om-abs ratios) size)))

(loop for n in new-size
 for x from 0 to (1- (length ratios))
 collect (if (member x rests-pos) (* -1 n) n))))  

(om::defmethod! glim ((ratios list) (minlim number) (maxlim number))
       :initvals '((1 1 1 1 1 1 1) 3 2)
       :indoc '("list" "number" "number") 
       :icon 01
       :doc "Scales the durations of  grtm by multiplying them with values
given by linear interpolation between minlim and maxlim.  For instance if minlim = 2
and maxlim 3 then the first duration is multiplied with 2 and the last one with 3.
Intermediate scaling values are gained by linear interpolation.
(1 1 1 1 1 1 1) when minlim = 3 and
maxlim = 2 -> (3 17/6 8/3 5/2 7/3 13/6 2)."
 (let ((interpol (om::interpolation minlim maxlim (length ratios) 0.0)))
 (mapcar #'rationalize (om::om* interpol ratios))))

;;;;;;;;;;;;
;; CONTOUR 
;; TODO



;;;;;;;;;;;;
;; PROCESS

(defun ratios-to-ms (ratios tempo)
 (let* ((whole-note (/ 240000 tempo)))
  (mapcar #'(lambda (x)
                    (coerce x 'double-float))
  (om* whole-note ratios))))

(om::defmethod! g-interpol ((ratios-begin list) (ratios-end list) (samples integer) (curve float) (bpm number) (time-sign list) (max-div integer) (forbid list) (precision float))
   :initvals '( (1/16 1/8 1/16 1/8 1/8) (1/4 1/4 1/4 1/4 1/4) 8 0.0
                 90 (4 4) 4 nil 1.0)
  	:indoc '("list" "list" "integer" "float" "number" "list" "integer" "list" "float") 
  	:icon 01       
  	:doc "Calculates an rhythmic interpolation between two lists of ratios in <n> samples.
The output is a voice object. 
The other arguments are: 
- curve 
- bpm (tempo) 
- time-signature
- max. subdivision
- forbidden subdivisions
- precision."
    (let* ((interpolation (om::interpolation (ratios-to-ms ratios-begin bpm) (ratios-to-ms ratios-end bpm) samples curve))
             (rationalize (mapcar #'rationalize (om::flat interpolation)))
             (quantization (om::omquantify rationalize
                                   bpm
                                   time-sign
                                   max-div
                                   forbid
                                   0
                                   precision)))
     (make-instance 'om::voice :tree quantization :tempo bpm)))


;;TODO: G-LAYERS


;; TODO: FIX G-MATRIX --> BEGIN1-END1 - BEGIN2-END2
(om::defmethod! g-matrix ((ratios-begin list) (ratios-end list) (levels integer) (samples integer) (lcurve float) (scurve float) (bpm number) (time-sign list) (max-div integer) (forbid list) (precision float))
   :initvals '( (1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16)
                (1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4)
                 4 8 0.0 0.0
                 60 (4 4) 4 nil 1.0)
  	:indoc '("list" "list" "integer" "float" "number" "list" "integer" "list" "float") 
  	:icon 01       
  	:doc "Calculates an rhythmic interpolation between two lists of ratios in <n> samples.
The output is a poly object. 
The other arguments are: 
- curve 
- bpm (tempo) 
- time-signature
- max. subdivision
- forbidden subdivisions
- precision."   
     (let* ((first-interpol (om::interpolation (ratios-to-ms ratios-begin bpm) (ratios-to-ms ratios-end bpm) samples scurve))
              (matrix  (om::mat-trans (loop for vert in (om::mat-trans (list (om::flat first-interpol) (reverse (om::flat first-interpol))))
                                           collect (om::flat (om::interpolation (list (car vert)) (cdr vert) levels lcurve)))))
             (rationalize (loop for interpol-ratios in matrix collect (mapcar #'rationalize (om::flat interpol-ratios))))
             (voices-quantization (loop for voice-ratios in rationalize
                                                       collect (make-instance 'om::voice :tree
                                                      (om::omquantify voice-ratios
                                                       bpm
                                                       time-sign
                                                       max-div
                                                       forbid
                                                       0
                                                       precision)
                                                      :tempo bpm))))
     (make-instance 'om::poly :voices voices-quantization)))

