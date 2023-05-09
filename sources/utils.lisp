(in-package :omlindberg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; THANKS TO CHARLES NEIMOG WHO FOUND THIS ON REEDIT

(defun list-of-listp (thing) (and (listp thing) (every #'listp thing)))
(deftype list-of-lists () '(satisfies list-of-listp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pcs-to-mcs (list)
(om::om+ 6000 (om::om* 100 list)))

(om::defmethod! pc->mc ((pc integer))
 :initvals '( 0 )
	:indoc '("pitch-classes") 
	:icon 01
	:doc "Converts pitch-classes into midicents around 6000 (middle C)"
(om::om+ 6000 (om::om* 100 pc)))

(om::defmethod! pc->mc ((pcs-list list))
 :initvals '( (0 1 3 4 6 9))
	:indoc '("pitch-classes") 
	:icon 01
	:doc "Converts a list of pitch-classes into a list of midicents around 6000 (middle C)"
(mapcar #'pc->mc pcs-list))
;(mapcar #'pcs-to-mcs pcs-list))

(defun mcs-to-pcs (chord-midics) 
(om::om/ (mapcar #'(lambda (input1)
          (mod input1 1200)) (approx-m chord-midics 2)) 100))

(om::defmethod! mc->pc ((midic integer))
 :initvals '( 6000)
	:indoc '("midicents") 
	:icon 01
	:doc "Midicents to pitch-classes"
(om::om/ (mod (om::approx-m midic 2) 1200) 100))

(om::defmethod! mc->pc ((mcs-list list))
 :initvals '( (6000 6100 6300 6400 6600 6900))
	:indoc '("midicents") 
	:icon 01
	:doc "Converts a list of midicents into a list of pitch-classes"
(mapcar #'mc->pc mcs-list))
;(mcs-to-pcs mcs-list))

(defun hex-complement (first-hexachord)
(let ((all-pcs (list 0 1 2 3 4 5 6 7 8 9 10 11))
      (chord-pcs (mc->pc first-hexachord)))
(om::x-diff all-pcs chord-pcs)))

(defun dist-mod-12 (input1 input2)
  (om::list-min (om::x-append (mod (- input1 input2) 12) (mod (- input2 input1) 12))))

(defun all-rotations (input1)
     (let ((nrot (om::arithm-ser 0 (- (length input1) 1) 1)))
       (mapcar #'(lambda (input2)
         (om::rotate input1 input2)) nrot))) 

(defun all-distances-mod-12 (list1 lists)
  (loop
     for list in lists
     for y = (mapcar #'(lambda (input1 input2)
               (dist-mod-12 input1 input2)) list1 list)
     collect y))

(defun set-complement-mcs-pcs (first-hexachord)
(let* ((all-pcs (list 0 1 2 3 4 5 6 7 8 9 10 11))
        (chord-pcs (mcs-to-pcs first-hexachord)))
      (om::x-diff all-pcs chord-pcs)))

(defun set-complement-pcs (first-hexachord)
(let ((all-pcs (list 0 1 2 3 4 5 6 7 8 9 10 11)))
      (om::x-diff all-pcs first-hexachord)))

(defun octaves-note (input1 input2)
   (let* ((distance (- input1 input2)) 
          (my-octave (om::om// distance 1200))
          (my-rest (- distance (* 1200 my-octave))))
(cond ((= my-rest 0) 
        (+ (* my-octave 1200) input2) (* 1200 (+ 1 my-octave)))
        (t (+ (* (+ 1 my-octave) 1200) input2)))))

(defun if-first-higher (list)
(cond ((> (first list) (second list))
          (octaves-note (first list) (second list)))
          (t (second list))))

(defun ascending-order (counter list1)
   (if (= counter (- (length list1) 2))
    (om::x-append (cddr list1) (first list1) (if-first-higher list1))     
    (ascending-order (+ counter 1) 
    (om::x-append (if-first-higher list1) (cddr list1) (first list1)))))

(defun ascending-chord (list-of-pcs)
 (let* ((list-of-mcs (pcs-to-mcs list-of-pcs))
       (my-counter 0))
 (ascending-order my-counter list-of-mcs)))

(defun inversions-down (chord-midics accumul)
 (let* ((reverse-rotation (reverse (om::rotate (reverse chord-midics))))
          (inv-down  
          (om::x-append  
            (om- (octaves-note (second reverse-rotation)
                                                               (first reverse-rotation))
            1200)
            (cdr reverse-rotation)))
          (flat-length  (length 
                              (flat 
                              (append accumul
                                       inv-down))))
          (results (om::list-explode (flat 
                                            (append accumul
                                             inv-down))
                        (/ flat-length (length chord-midics)))))
  (if (= (length results) (length chord-midics))
       results
       (inversions-down (om::last-elem results) results))))

(defun harm-inv (list-of-midics)
 (let ((my-rotations (all-rotations list-of-midics)))
   (mapcar #' (lambda (input1)
     (ascending-order 0 input1)) my-rotations))) 

(om::defmethod! first-last-6 ((chord list))
 :initvals '( (4800 5200 5500 6000 6400 6700 7200 7600 7900 8400 8800 9100))
	:indoc '("list-of-midicents") 
	:icon 01
	:doc "Returns the first and last 6 elements of a list."
        :numouts 2
(let* ((first-6 (om::first-n chord 6))
       (last-6 (om::last-n chord 6)))
(values first-6 last-6))) 

(om::defmethod! intervals->chord ((lowest-note string) (intervals list))
        :initvals '("eb2" (4 2 1 3 3 1 3 3 1 2 4))
	:indoc '("pitch" "list") 
	:icon 01
	:doc "Returns a chord constructed by a note and a list of intervals."
(let* ((note-mc (om::n->mc lowest-note))
        (list-of-intervals  (om::om* 100 intervals)))
(om::dx->x note-mc list-of-intervals)))

(om::defmethod! chord->intervals ((chord list))
        :initvals '((4000 4700 5400 5800 6100 6500 6800 7200 7500 7900 8600 9300))
	:indoc '("midicents") 
	:icon 01
	:doc "Returns a list of intervals from chord."
(if (list-of-listp chord)
   (mapcar #'(lambda (input1) (om::om/ (om::x->dx input1) 100)) chord)
   (om::om/ (om::x->dx chord) 100)))

(defun chord-seq-to-bpf-lib (chords-midics)
 (if (not (list-of-listp chords-midics))
      (error "The input argument should be a list of lists of midicents.")
 (let* ((y-points-list (om::mat-trans chords-midics)))
  (mapcar #'(lambda (input1)
          (om::simple-bpf-from-list nil input1)) y-points-list))))

(om::defmethod! chord-seq->bpf-lib ((midics list))
        :initvals '(nil)
	:indoc '("list-of-lists of midicents") 
	:icon 01
	:doc "Constructs a BPF-LIB object from a list-of-lists of midicents. All chords must have the same number of notes."
(om::make-instance 'om::bpf-lib :bpf-list (chord-seq-to-bpf-lib midics)))

(om::defmethod! mod12 ((num integer))
        :initvals '(-4)
	:indoc '("pitch-class") 
	:icon 01
	:doc "Integer or list of integers modulo 12."
(mod num 12))

(om::defmethod! mod12 ((numbers list))
        :initvals '(-4)
	:indoc '("pitch-classes") 
	:icon 01
	:doc "Integer or list of integers modulo 12."
(mapcar #'mod12 numbers)) 

(om::defmethod! pc->chord ((pcs list) (octave integer))
        :initvals '((0 1 3 4 6 9) 3)
	:indoc '("pitch-classes" "octave") 
	:icon 01
	:doc "Returns a chord constructed by list of pitch-classes and a octave."
 (om::dx->x (+ (* 1200 octave)
                        (* 100 (first pcs)))
                    (om::om* 100 
                                    (mod12 (x->dx (transp-to-zero pcs))))))

(om::defmethod! rotations ((a-list list))
        :initvals '( (0 1 2 3 4 5) )
	:indoc '("Any list.") 
	:icon 01
	:doc "Calculates all rotations of a given list."
(all-rotations a-list))



