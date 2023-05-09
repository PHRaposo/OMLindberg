(in-package :omlindberg)

(defun common-notes-progressions (chords accumul)
	(let* ((first-pcs (mc->pc (first chords)))
	       (second-pcs (mc->pc (second chords)))
		   (common-pcs (om::x-intersect first-pcs second-pcs)) 
		   (first-diff (om::x-diff first-pcs common-pcs)) 
		   (second-diff (om::x-diff second-pcs common-pcs)) 
		   (diff-notes (om::posn-match (second chords)
		                                      (om::flat (mapcar #'(lambda (input)
											              (tristan-positions second-pcs input))
														 common-pcs))))
		  (first-without-diff (om::posn-match (first chords)
		                                      (om::flat (mapcar #'(lambda (input)
											              (tristan-positions first-pcs input))
														 first-diff))))
   		  (second-without-diff (om::posn-match (second chords)
   		                                      (om::flat (mapcar #'(lambda (input)
   											              (tristan-positions second-pcs input))
   														 second-diff))))
		 (results
	         (om::x-append accumul
			            (list (first chords))
		                    (list second-without-diff)					   	             
                                    (list (second chords)) 
			            (list first-without-diff)))) 

								
	(if (= 2 (length chords))														 
		      (write (remove nil results))
	     (common-notes-progressions (cdr chords) results))))
		  
(om::defmethod! superimpose ((chords list))
 :initvals '( ((6000 6600 7300 7600) (6400 7000 7700 8000) 
	           (5800 6400 7100 7400) (6600 7200 7900 8200)
			   (7100 7700 8400 8700) (6900 7500 8200 8500) 
			   (6200 6800 7500 7800) (6300 6900 7600 7900)))
	:indoc '("list-of-midicents") 
	:icon 01
	:doc "Returns a chord progression by common tones. Each pair of chords will produce 4 results: 
	- first chord;
	- sum of the two chords, where the octaves of the SECOND chord are ommited;
	- a NEW chord (or single notes) with the difference between the two chords;
	- sum of the two chords, where the octaves of the FIRST chord are ommited and REPLACED by the notes from the difference."
 (common-notes-progressions chords nil))

(om::defmethod! freeze ((chord list) (positions list) (chords list))
 :initvals '( (6000 6200 6600 6900 7100 7500 7600 8000 8200 8500 8900 9100) ;EXAMPLE FROM ML's TWINE - chord-3
                   (0 1 2 3 4 5) ;positions - first 6 notes of the chord
                 ((3800 4700 4900 5200 5800 6000 6700 6900 7500 7800 8000 8900) ;MAIN CHORDS 
                  (2800 3600 3700 4100 4600 4700 5600 5700 6200 6600 6700 7500) 
                  (6000 6200 6600 6900 7100 7500 7600 8000 8200 8500 8900 9100) 
                  (6900 7100 7500 8000 8600 8800 8900 9100 9700 10200 10600 10800)
                  (3900 4500 4900 5000 5400 5600 6500 6700 7100 7200 7600 8200)
                  (3900 4100 4700 5200 5500 5600 5700 5800 6100 6600 7200 7400)
                  (2100 2700 2900 3000 3500 4300 4800 5600 6100 6200 6400 7000)
                  (3700 4600 5000 5400 5600 6000 6500 6900 7100 7500 7900 8800))
 )
	:indoc '("list-of-midicents" "list-of-positions" "list-of-lists-of-midicents") 
	:icon 01
	:doc "Returns a chord progression using the 'freeze' technique."

 (let ((fixed-notes (om::posn-match chord positions))) 
  (mapcar #'(lambda (input-out)
   (om::sort-list (om::x-append (om::posn-match input-out
                                                      (mapcar #'(lambda (input-in)
                                                       (position input-in
                                                                     (mc->pc input-out)))
                                                     (om::x-diff (mc->pc input-out)
                                                                       (mc->pc fixed-notes))))
                           fixed-notes)))
   chords)))

(defun low-filter (note lowest)
 (if (< note lowest) 
     (+ lowest (* 100 (oml::mod12 (- (oml::mc->pc note) (oml::mc->pc lowest)))))
     note))

(defun high-filter (note highest)
 (if (> note highest) 
     (- highest (* 100 (oml::mod12 (- (oml::mc->pc highest) (oml::mc->pc note)))))
     note))

(defun auto-transposer-up (chord)
(mapcar #'(lambda (input1)
 (om::sort-list (om::dx->x input1 (om::x->dx chord)))) chord))

(defun auto-transposer-down (chord)
(mapcar #'(lambda (input1)
 (om::sort-list (om::dx->x input1 (om::x->dx (reverse chord))))) (reverse chord)))

(om::defmethod! auto-transp ((chord list))
 :initvals '( (3800 4700 4900 5200 5800 6000 6700 6900 7500 7800 8000 8900)) ;first chord from Lindberg's Twine
	:indoc '("list-of-midicents") 
	:icon 01
	:doc "Returns the transpositions of a given chord by itself, upwards and downwards, with the lowest and highest note as a treshold."
        :numouts 2
(let* ((transp-up (auto-transposer-up chord))
        (transp-down (auto-transposer-down chord))
        (auto-transp-down 
         (mapcar #'sort-list
          (loop for transp in transp-down
                   for y = (mapcar #'(lambda (input1) 
                              (low-filter input1 (first chord))) transp)
                   collect y)))
        (auto-transp-up
         (mapcar #'sort-list
          (loop for transp in transp-up
                   for y = (mapcar #'(lambda (input1) 
                              (high-filter input1 (om::last-elem chord))) transp)
                   collect y))))
(values auto-transp-up auto-transp-down)))