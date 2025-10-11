(in-package :omlindberg)

(om::defmethod! ml-interpolation ((chord1 list) (chord2 list) (samples number) (type symbol))
 :initvals '( (3800 4700 4900 5200 5800 6000) (6700 6900 7500 7800 8000 8900) 10 :sinusoidal)
	:indoc '("chord1" " chord2" "n-samples" "linear-or-sinusoidal") 
	:icon 01
        :menuins '( (3 (("linear" :linear) ("sinusoidal" :sinusoidal))))
	:doc "Calculates the interpolation between two chords, <chord1> and <chord2>, in a number of samples, according to selected type (linear or sinusoidal)."
(let* ((linear (mapcar #'(lambda (input1)
        (om::om/ input1 (om::om- samples 1))) (om::arithm-ser 0 (om::om- samples 1) 1)))
       (sinusoidal (mapcar #'(lambda (input1) 
        (om::om* 1/2 (om+ 1 (sin (om::om+ (om::om/ (om::om* 3 pi) 2) (om::om* input1 pi)))))) linear)))

(cond ((equal type :linear)     
 (om::mat-trans  
  (om::om+ chord1
   (om::om* 100 
    (om::om-round
     (om::om-round
     (mapcar #'(lambda (input1)         
       (om::om* linear input1)) (om::om- chord2 chord1))
      1 1) 
      0 100)))))
 
((equal type :sinusoidal)
(om::mat-trans  
  (om::om+ chord1
   (om::om* 100 
    (om::om-round 
     (om::om-round
      (mapcar #'(lambda (input1)         
       (om::om* sinusoidal input1)) (om::om- chord2 chord1))
      1 1) 
     0 100)))))
(t nil))))

;; INTERPOLATION WITH FUNCTION f(x) = FUN 

(defun interpol-fn (begin end samples fn count)
(if (= count (1- samples))
    (list end)
 (cons (+ begin (* (apply fn (list (/ count (1- samples)))) (- end begin)))
       (interpol-fn begin end samples fn (1+ count)))))

(om::defmethod! interpolation-fun ((begin list) (end list) (samples integer) &optional (function function))
  :initvals '((3600 4000 4500 5000) (7200 7600 7900 8400) 20 nil)
  :icon 01
  :indoc '("number or list" "number or list" "integer" "number")
  :doc "Interpolates 2 numbers or lists (from <begin> to <end>) through <samples> steps.

<function> is f(x)=x for linear (lambda patch) or any other function.
" 
(let ((fun (if function function (curve-fun 0.0))))
  (om::mat-trans
   (mapcar #'(lambda (n1 n2)
               (interpol-fn n1 n2 samples fun 0))
    begin end))))

(om::defmethod! curve-fun ((curve float))
  :initvals '(0.0)
  :icon 01
  :indoc '("float")
  :doc "Generates a function with selected curve."
(eval `(lambda (x) (expt x (exp ,curve)))))

(om::defmethod! sinus-fun ()
  :icon 01
  :doc "Generates a sinusoidal function"
(eval `(lambda (x)
         (* 1/2 (+ 1 (sin (+ (/ (* 3 pi) 2) (* pi x))))))))
		 

