(in-package :omlindberg)

(om::defmethod! harmonic-ttch ((note number)) 
	:initvals '( 3600)
	:indoc '("midic") 
	:icon 01
    :doc "Returns a harmonic twelve tone chord constructed with the partials 1 3 5 7 9 11 13 15 17 19 21 27 of the given note 'midics'."
(om::approx-m 
 (om::f->mc
  (om::om* '(1 3 5 7 9 11 13 15 17 19 21 27)
           (om::mc->f note)))
 2)) 

(om::defmethod! harmonic-ttch ((notes list)) 
	:initvals '( 3600)
	:indoc '("midic") 
	:icon 01
    :doc "Returns a harmonic twelve tone chord constructed with the partials 1 3 5 7 9 11 13 15 17 19 21 27."
(mapcar #'harmonic-ttch notes))

(defun harmonic-s (fundamental partials approx)
 (om::approx-m  
  (om::f->mc 
   (om::om* (om::mc->f fundamental) partials))
approx))

(om::defmethod! harmonic-series ((fundamental number) (partials list) (approx integer)) 
	:initvals '( 2400 (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16) 16)
	:indoc '("midics" "list" "integer") 
	:icon 01
    :doc "Returns a harmonic series from a <fundamental>, <partials> and <approximation> (2 for semitones)."
(harmonic-s fundamental partials approx))

#|
(defun possible-harmonic (chord-midics)
 (let* ((sub-harmonic 
         (mapcar #'(lambda (input)
          (om::om/ input '(1 3 5 7 9 11 13 15 17 19 21 27)))
         (om::mc->f chord-midics)))
        (app (remove-duplicates
                 (om::sort-list
                  (om::approx-m 
                  (flat
                  (om::f->mc sub-harmonic)) 2)))))
 (mapcar #'(lambda (input)
 (harmonic-ttch input))
  app)))

(defun n-common-tones (chord possible-harmonic n)
 (let* ((intersect (mapcar #'(lambda (input)
                           (om::x-intersect input chord)) possible-harmonic))
          (common (mapcar #'length intersect))
          (greater-than-n (remove-duplicates 
                                    (loop for x in common
                                    if (>= x n)
                                   collect x)))
          (positions (flat (mapcar #'(lambda (input)
                            (tristan-positions common input)) greater-than-n))))
(om::posn-match possible-harmonic positions)))

(defun fund-common (chord results)
  (remove-duplicates 
   (om::x-append (first results)
                     (om::x-intersect results chord))))
|#

(om::defmethod! WTVF ((chord list) (selection symbol) (mc-min integer) (mc-max integer) (n integer)) ;NEW (10/06/2024)
	:initvals '( (5100 5500 5700 5800 6100 6400 6500 6800 7100 7200 7400 7800) :highest 1200 12000 12)
	:indoc '("chord-midics" "highest or all" "min-midicents" "max-midicents" "n-common-tones" ) 
	:icon 01
    :menuins '( (1 (("highest" :highest) ("all" :all) )))
	:numouts 4
    :doc "Well Tempered Virtual Fundamental: calculates all harmonic twelve tone chords thats shares 'n' or higher common tones with the given chord.
	The first output returns the fundamental with common tones.
	The second output returns the complete harmonic series, up to partial 40.
	The third output returns the fundamental with common tones with odd partials (harmonic tweleve tone chord).
	The fourth output returns the complete harmonic series with odd partials."
	(let ((res (screamer::well-tempered-virt-fun chord mc-min mc-max n)))
	 (cond ((not (null res))
	        (if (equal selection :all)
	            (values res
				       (loop for r in res
						     collect (remove-duplicates(om::approx-m (om::f->mc (om::om* (om::mc->f (car r)) (om::arithm-ser 1 40 1))) 2)))
	  				   (loop for r in res
	  						 collect (intersection (harmonic-ttch (car r)) chord))
	  	  			   (loop for r in res
	  	  					 collect (harmonic-ttch (car r))))								  	 							 
		        (values (car res)
			           (remove-duplicates (om::approx-m (om::f->mc (om::om* (om::mc->f (car (car res))) (om::arithm-ser 1 40 1))) 2))
					   (reverse (intersection (harmonic-ttch (car (car res))) chord))
					   (harmonic-ttch (car (car res))))))
		  (t (om::om-message-dialog "UNABLE TO FIND A FUNDAMENTAL.") (om::om-abort)))))
		 
#|	
 (let* ((results (if (= n 12) (wtvf-recursive chord n mc-min mc-max) (well-temp-vf chord n mc-min mc-max)))
          (highest (if (not (null results)) (highest-fund results) nil)))

  (cond ((null results) (progn (om::om-message-dialog "ERROR: Unable to find a fundamental.") (om::om-abort)))
            ((and (equal selection :all) (> (length results) 1)) (values results (mapcar #'(lambda (input1) (fund-common chord input1)) results)))
            ((and (equal selection :highest)  (> (length results) 1)) (values highest (fund-common chord highest)))
            (t (if (equal selection :all) 
                    (values results (fund-common chord results))
                    (values highest (fund-common chord highest))))))) 

(defun wtvf-recursive (chord n mc-min mc-max)
 (let ((results (well-temp-vf chord n mc-min mc-max))) 
 (if (null results) (wtvf-recursive chord (1- n) mc-min mc-max) results)))  

(defun well-temp-vf (chord n mc-min mc-max)
(let* ((poss-harm (possible-harmonic chord))
         (comm-ton (n-common-tones chord poss-harm n))
         (results (loop for x in comm-ton
                               if (and (>= (first x) mc-min)
                                          (<= (first x) mc-max))
                                  collect x into included
                              else
                                 collect x into excluded
                             finally (return included))))
(write results)))

(defun highest-fund (results)
 (let ((fundamentals (mapcar #'first results)))
  (nth (position (om::list-max fundamentals) fundamentals) results)))
|#

