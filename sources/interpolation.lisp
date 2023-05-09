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
