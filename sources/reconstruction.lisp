(in-package :omlindberg)

(defun length-positions (list1 list2)
(let* ((list-repet (mapcar #'(lambda (input1)
        (tristan-positions list1 input1)) list2)))
(mapcar #'(lambda (input2)
          (length input2)) list-repet))) 

(defun distances-pcs-mod12 (pc-max pcs-out)
 (om::list-min (om::x-append 
                       (mod12 (om::om- pc-max pcs-out))
                       (mod12 (om::om- pcs-out pc-max)))))

(defun transp-to-zero (pcs)
  (mod12 (om::om- pcs (first pcs))))

(defun sum-of-list (list)
 (apply '+ list))

(om::defmethod! applysum ((pcs list))       
    :initvals '((1 3 4 6 7 9))
	:indoc '("list of pcs") 
	:icon 01
	:doc "sum of list or list of lists"
(if (list-of-listp pcs) (mapcar #'sum-of-list pcs) (sum-of-list pcs)))

(defun dist-class (pc1 pc2) 
(let* ((k (om::list-min (list (length pc1) (length pc2))))
        (n (om::first-n pc1 k))
        (p (om::first-n pc2 k)))
 (om::om/ 
  (sqrt 
   (applysum
    (loop for nx in n
             for px in p
             for j = (expt (- nx px) 2)
             collect j))) 
   k)))

(om::defmethod! dist-classes ((pc1 list) (pc2 list))
        :initvals '( (0 1 2 3 4 5) (0 1 3 5 7 9) )
	:indoc '("integers" "integers") 
	:icon 01
	:doc 
"Calculates distance between two pitch-classes using the calculations presented in the Marc Chemillier's article.

                  2              2              2
sqrt((a + b ) + (a + b ) ... (a + b ) )
         1    1         2    2        n    n     
_________________________
                       k

a = n elements of sc1
b = n elements of sc2
k = min. of a and b
"

(dist-class pc1 pc2))

(defun flatten-chord-new-test (chord)
(let* ((chord-pcs (mcs-to-pcs chord))
         (all-pcs (list 0 1 2 3 4 5 6 7 8 9 10 11))
      (pcs-positions (length-positions chord-pcs all-pcs)))
   (if (= (om::list-max pcs-positions) 1)
        (write chord)
    (let* ((min-pcs (om::posn-match all-pcs (tristan-positions pcs-positions (om::list-min pcs-positions))))
            (max-pcs (tristan-positions 
                   pcs-positions 
                   (om::list-max (remove-duplicates (remove 'nil 
                                                                                      (mapcar #'(lambda (input1)
                                                                                       (if (> input1 1) (write input1))) pcs-positions))))))
             (min-distance-pc (if (> (length max-pcs) 1) 
                                  (let* ((distances (mapcar #'(lambda (input1)
                                        (distances-pcs-mod12 input1 min-pcs)) max-pcs)))

      (nth (position (om::list-min distances) distances) max-pcs))                     
    (first max-pcs)))

      (distances (mapcar #'(lambda (input1)
                              (distances-pcs-mod12 input1 min-distance-pc)) min-pcs))   
     (closest-pc-out-position (tristan-positions distances (om::list-min distances)))
     (new-pcs (om::posn-match min-pcs closest-pc-out-position))
     (max-pcs-position (tristan-positions chord-pcs min-distance-pc))
     (new-chord-pcs 
      (let ((results
       (if (> (length max-pcs-position) 1) 
           (if (> (length new-pcs) 1) 
               (mapcar #'(lambda (outer-input)
                (mapcar #'(lambda (input1) (om::flat (om::subs-posn chord-pcs input1 outer-input))) max-pcs-position)) new-pcs)
              (mapcar #'(lambda (input1) (om::flat (om::subs-posn chord-pcs input1 new-pcs))) max-pcs-position))
      (om::subs-posn chord-pcs max-pcs-position new-pcs))))
 (if (> (length new-pcs) 1) (om::flat-once results) results)))
     (new-chord 
     (if (= (length new-chord-pcs) 1)
             (approx-oct chord new-chord-pcs)
              (let* ((new-chords (mapcar #'(lambda (input1) (approx-oct chord input1)) new-chord-pcs))
                      (new-chords-ints (mapcar #'(lambda (input1) (applysum (om::om-abs (om::x->dx input1)))) new-chords))
                      (max-position (position (om::list-max new-chords-ints) new-chords-ints))) 
              (nth max-position new-chords))))) ;;; MAX. OF SUM OF THE INTERVALS ???
     (flatten-chord-new-test new-chord)))))

;;; THIS VERSION PRODUCES THE EXACTLY SAME RESULTS AS THE MARC-CHEMILIER'S EXAMPLE ;;;
;;; APPLIED TO MIDICENTS ;;; 04/24/2023

(om::defmethod! flatten ((chord list) &optional (sort-list? :no))
        :initvals '( (4800 6000 7200 8400) :no)
	:indoc '("list or list-of-lists of midicents") 
	:icon 01
:menuins '( (1 (("yes" :yes) ("no" :no))))
	:doc "Creates a new chord without doublings through approximations."
 (cond ((list-of-listp chord)
            (if (equal sort-list? :yes) 
                (mapcar #'(lambda (input1) (om::sort-list (flatten-chord-new-test input1))) chord)
               (mapcar #'flatten-chord-new-test chord)))
  (t  (if (equal sort-list? :yes)
           (om::sort-list (flatten-chord-new-test chord))
           (flatten-chord-new-test chord)))))

(defun flatten-pc (chord-pcs) ;;; SAME FUNCTION APPLIED TO PCS ;;;
(let* ((all-pcs (list 0 1 2 3 4 5 6 7 8 9 10 11))
      (pcs-positions (length-positions chord-pcs all-pcs)))
   (if (= (om::list-max pcs-positions) 1)
        chord-pcs
    (let* ((min-pcs (om::posn-match all-pcs (tristan-positions pcs-positions (om::list-min pcs-positions))))
            (max-pcs (tristan-positions 
                   pcs-positions 
                   (om::list-max (remove-duplicates (remove 'nil 
                                                                                      (mapcar #'(lambda (input1)
                                                                                       (if (> input1 1) (write input1))) pcs-positions))))))
             (min-distance-pc (if (> (length max-pcs) 1) 
                                  (let* ((distances (mapcar #'(lambda (input1)
                                        (distances-pcs-mod12 input1 min-pcs)) max-pcs)))

      (nth (position (om::list-min distances) distances) max-pcs))                     
    (first max-pcs)))

      (distances (mapcar #'(lambda (input1)
                              (distances-pcs-mod12 input1 min-distance-pc)) min-pcs))   
     (closest-pc-out-position (tristan-positions distances (om::list-min distances)))
     (new-pcs (om::posn-match min-pcs closest-pc-out-position))
     (max-pcs-position (tristan-positions chord-pcs min-distance-pc))
     (new-chord-pcs 
      (let ((results
       (if (> (length max-pcs-position) 1) 
           (if (> (length new-pcs) 1) 
               (mapcar #'(lambda (outer-input)
                (mapcar #'(lambda (input1) (om::flat (om::subs-posn chord-pcs input1 outer-input))) max-pcs-position)) new-pcs)
              (mapcar #'(lambda (input1) (om::flat (om::subs-posn chord-pcs input1 new-pcs))) max-pcs-position))
      (om::subs-posn chord-pcs max-pcs-position new-pcs))))
      (if (> (length new-pcs) 1) (om::flat-once results) results)))
     (max-mod12-ints 
     (if (= (length new-chord-pcs) 1)
              new-chord-pcs
              (let* ((ints (mapcar #'(lambda (input1) (applysum (mod12 (om::x->dx input1)))) new-chord-pcs)) ;;;;MOD12 INSTEAD OF ABS
                      (max-position (position (om::list-max ints) ints))) 
                      (nth max-position new-chord-pcs))))) ;;; MAX. OF SUM OF THE INTERVALS (MOD12) ???
     (flatten-pc max-mod12-ints)))))

;;; ================================ ;;;
;;; FROM OM-TRISTAN 3.4 ;;;

(defun tristan-positions (input-list input-elem)
  (let ((index 0) res)
    (dolist (n input-list)
      (if (equal input-elem n)  (push index res)) ;modified to equal
      (setq index (1+ index)))
    (nreverse res)))

;;; ================================ ;;;

(om::defmethod! par ((chord list))
        :initvals '( (3600 3900 4100 4200 4400 4800)) ;Chemillier's example (in midicents).
	:indoc '("list in midicents") 
	:icon 01
	:doc "Calculates all the parameters of a given chord. The first output prints the results and the second output is a list with these parameters."
        :numouts 2
 (let* ((oct (om::om// chord 1200))
          (pth (mc->pc chord)) ;pitch-classes
          (pth-asc (om::sort-list pth)) ;pc in ascending order
          (pth-uni (remove-duplicates pth-asc)) ;remove pcs duplicates
          (ord (om::posn-order pth '<)) ;order of the pcs in the chord
          (pth-cnt (length-positions pth-asc pth-uni)) ;number of ocurrences of each pc
          (normal (normal-order pth-uni)) ;normal order
          (pr-dir (prime1 pth-uni)) ;direct prime form
          (pr (prime pth-uni)) ;prime form
          (fl1 (if (equal pr pr-dir) 
                    (position (first normal) pth-uni) ;equivalent position of the firt note of the prime form in the pth-uni list.
                    (position 0 (reverse (om::n-ord :integer (om::inv :integer pr)))))) ;equivalent position of the first note of the prime-form in the reverse of normal-order list.
          (fl2  (if (equal pr pr-dir) 0 1))
          (first (if (equal pr pr-dir) (first normal) (last-elem normal)))
          (ref  (if (equal pr pr-dir) (mapcar #'(lambda (input) (position input normal)) pth-uni)
                                               (mapcar #'(lambda (input) (position input (reverse normal))) pth-uni))) ;list of positions of pth-uni in the normal order list (pr = prdir) or reverse of the normal order (pr /= prdir)
)
(values (list chord oct pth pth-asc ord pth-cnt pth-uni pr normal fl1 fl2 first ref)
(format nil "~% 
midics: ~A 
oct: ~A
pth: ~A 
pth-asc: ~A
ord: ~A
------------------------- 
pth-cnt: ~A
pth-uni: ~A
------------------------- 
pr: ~A
normal:~A
fl1: ~A 
fl2: ~A
-------------------------
first: ~A
ref: ~A" chord oct pth pth-asc ord pth-cnt pth-uni pr normal fl1 fl2 first ref))))

(defun gt-new-final (chord set-class)
 (let* ((chord-pcs (mc->pc chord)) ;pitch-classes
          (pth-asc (om::sort-list chord-pcs)) ;pc in ascending order
          (pth-uni (remove-duplicates pth-asc)) ;remove pcs duplicates
          (ord (om::posn-order chord-pcs '<)) ;order of the pcs in the chord
          (pth-cnt (length-positions pth-asc pth-uni)) ;number of ocurrences of each pc
          (normal (normal-order pth-uni)) ;normal order
          (pr-dir (prime1 pth-uni)) ;direct prime form
          (pr (prime pth-uni)) ;prime form
          (fl1 (if (equal pr pr-dir) 
                    (position (first normal) pth-uni) ;equivalent position of the first note of the prime-form in the pth-uni list.
                    (position 0 (reverse (om::n-ord :integer (om::inv :integer pr)))))) ;equivalent position of the first note of the prime-form in the reverse of the normal-order [not pth-uni list] - (reverse->normal-order->inversion->pr)
          (first (if (equal pr pr-dir) (first normal) (nth fl1 (reverse normal))))
          (ref  (if (equal pr pr-dir) (mapcar #'(lambda (input) (position input normal)) pth-uni)
                                               (mapcar #'(lambda (input) (position input (reverse normal))) pth-uni))) ;list of positions of pth-uni in the normal order list (pr = prdir) or reverse of the normal order (pr /= prdir)

;;;;;RECONSTRUCTION OF THE CHORD WITH THE NEW SET-CLASS;;;;

          (new-normal (if (equal pr pr-dir) 
                                    (mod12 (om::om+ (first normal) set-class))
                                    (om::rotate (mod12 (om::om- (nth fl1 (reverse normal)) set-class)) (* -1  fl1)))) ;corrects set-classes when inversion of of the prime-form(A) /= direct prime-form (B)
           (new-pth-uni (om::posn-match new-normal ref))
           (new-pth-asc (om::flat (mapcar #'(lambda (input1 input2) (om::repeat-n input1 input2)) new-pth-uni pth-cnt)))
           (new-pth (om::posn-match new-pth-asc ord)))

(mapcar #'(lambda (input1 input2) (fix-octaves-note input1 input2)) chord new-pth)))

(om::defmethod! get-new ((midics list) (pcs om::t))
 :initvals '( (3700 3900 4100 4200 4400 4900) (0 2 3 5 7))
	:indoc '("list or list of lists of midicents'" "pc-set or set-class") 
	:icon 01      
	:doc "Reconstructs a given chord (or chords) by imposing the choosen set-class."
(if (symbolp pcs) 
    (if (list-of-listp midics) 
        (mapcar #'(lambda (input1) (gt-new-final input1 (pc pcs))) midics)
         (gt-new-final midics (pc pcs)))

    (if (list-of-listp midics)
        (mapcar #'(lambda (input1)
         (gt-new-final input1 pcs)) midics)
        (gt-new-final midics pcs))))

(om::defmethod! approx-oct ((midic number) (pc integer))
 :initvals '( 3600 1)
	:indoc '("midic'" "pc") 
	:icon 01    
	:doc "Approximation of octaves."
 (fix-octaves-note midic pc))

(om::defmethod* approx-oct ((midics list) (pcs list))
 (mapcar #'(lambda (input1 input2) (fix-octaves-note input1 input2)) midics pcs))

(defun fix-octaves-note (input1 input2)
  (let* ((distances (- input1 (* 100 input2))) 
         (oct-rest  (multiple-value-bind (x y) (om// distances 1200) (list x y))))
  (if (>= (second oct-rest) 600) 
       (+ (* 100 input2) (* 1200 (+ 1 (first oct-rest) )))
       (+ (* 100 input2) (* 1200 (first oct-rest)))))) 

(om::defmethod! chain-get-new ((chords list) (set-class om::t) (mode symbol) (duplications symbol) &optional (twelve-tone :no))
 :initvals '( ((3800 4700 4900 5200 5800 6000) (3900 4800 5000 5300 5900 6100) (4100 5000 5200 5500 6100 6300) (4500 5300 5600 5900 6400 6700) (5000 5600 6000 6300 6700 7200) (5500 6000 6400 6700 7100 7700) (6000 6400 6900 7200 7500 8200) (6400 6600 7200 7500 7700 8600) (6600 6800 7400 7700 7900 8800) (6700 6900 7500 7800 8000 8900)) (om::6-2 om::6-Z10) :circular :flatten :no) ;Example by Marc Chemillier
	:indoc '("list or list of lists of midicents'" "set-class" "mode" "dup-notes" "twelve-tone?") 
	:icon 01
    :menuins '( (2 (("linear" :linear) ("circular" :circular))) (3 (("flatten" :flatten) ("original" :original))) (4 (("no" :no) ("yes" :yes))))        
	:doc "Reconstructs a list of chords <midicents> by imposing the choosen set-class."
(if (equal twelve-tone :yes) 
 (cond ((equal mode :circular)
  (if (not (listp set-class)) (write "ERROR: Can not build a circular list with a single set-class.")
     (let* ((n-repetitions (om::om// (length chords) (length set-class)))
            (repet-rest (- (length chords) (* n-repetitions (length set-class))))
            (circular-list (flat                              
                               (if (> repet-rest 0)
                                  (om::x-append (repeat-n set-class n-repetitions)
                                                         (first-n set-class repet-rest))
                                  (repeat-n set-class n-repetitions)))))
      (get-new-twelve-tone chords circular-list))))

            ((equal mode :linear) 
             (get-new-twelve-tone chords set-class))
  (t nil))

(let ((midics (if (equal duplications :flatten) (flatten chords)
                     chords)))

 (cond ((equal mode :circular)
  (if (not (listp set-class)) (write "ERROR: Can not build a circular list with a single set-class.")
     (let* ((n-repetitions (om::om// (length midics) (length set-class)))
            (repet-rest (- (length midics) (* n-repetitions (length set-class))))
            (circular-list (om::flat                              
                               (if (> repet-rest 0)
                                  (om::x-append (repeat-n set-class n-repetitions)
                                                         (first-n set-class repet-rest))
                                  (repeat-n set-class n-repetitions)))))
     (mapcar #'(lambda (input1 input2)
      (get-new input1 input2)) midics circular-list))))

            ((equal mode :linear) (get-new midics set-class))

           (t nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; RECONSTRUCTION WITH TWELVE-TONE CHORDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(om::defmethod! get-new-twelve-tone ((midics list) (set-class om::t))
 :initvals '( (3700 3900 4100 4200 4400 4900) om::6-2)
	:indoc '("list or list of lists of midicents'" "pc-set or set-class[fn]") 
	:icon 01      
	:doc "Reconstructs a given chord (or chords) by imposing the choosen set-class and its complement to generate a twelve-tone chord."
(cond ((and (list-of-listp midics) (symbolp set-class))
           (mapcar #'(lambda (input1) (gt-new-tt input1 (pc set-class))) midics))

          ((and (list-of-listp midics) (listp set-class))
           (mapcar #'(lambda (input1 input2) (gt-new-tt input1 (pc input2))) midics set-class))

          ((and (listp midics) (listp set-class))
           (mapcar #'(lambda (input1) (gt-new-tt midics (pc input1))) set-class))

   (t (gt-new-tt midics (pc set-class)))))

(defun best-rotation-transposition (normal1 normal2 sc comp)
(loop for j from 0 to 11
 for y = (loop for x from 0 to 11
                     do (let* ((rotation1 (best-rotation-pcs normal1 (mod12 (om::om+ x sc))))
                                   (rotation2 (best-rotation-pcs normal2 (mod12 (om::om+ x comp))))
                                   (distances1 (mapcar #'dist-mod-12 normal1 rotation1))
                                   (distances2 (mapcar #'dist-mod-12 normal2 rotation2)))

                    (when (and (<= (applysum distances1) j )
                                       (<= (om::list-max distances1) j )
                                       (<= (applysum distances2) j )
                                       (<= (om::list-max distances2) j ))               
  (return (om::x-append (list rotation1)
                                     (list rotation2))))))
when y return y))

(defun best-rotation-pcs (pcs1 pcs2)
(loop for j from 0 to 11
 for y = (loop for x from 0 to (1- (length pcs2))
                     when (and (<= (/ (applysum (mapcar #'dist-mod-12 pcs1 (om::rotate pcs2 x))) (length pcs2)) j )  
                                       (<= (om::list-max (mapcar #'dist-mod-12 pcs1 (om::rotate pcs2 x))) j )) 
                     return (om::rotate pcs2 x))
when y return y))

(defun get-inv-comp (pcs)
 (let ((inversion (om::inv :integer pcs)))
 (loop for x from 0 to 11 
          do (setq transp (mod12 (om::om+ x inversion)))
          (when (not (intersection pcs transp)) 
          (return (reverse transp))))))

(defun gt-new-tt (chord set-class)
 (let* ((first-hex (om::first-n chord 6))
          (second-hex (om::last-n chord 6))
          (par1 (par first-hex))
          (par2 (par second-hex))
          (flat-hex1 (om::sort-list (flatten-pc (fourth par1))))
          (flat-hex2 (om::sort-list (flatten-pc (fourth par2))))
          (par-pc1 (par-pc flat-hex1))
          (par-pc2 (par-pc flat-hex2)))

 (let* ((compl (get-inv-comp set-class))
          (new-transp-rot (best-rotation-transposition flat-hex1 flat-hex2 set-class (om::sort-list compl) ))  
          (new-hex1 (om::posn-match (om::sort-list (first new-transp-rot)) (fifth par1)))
          (new-hex2 (om::posn-match (om::sort-list (second new-transp-rot)) (fifth par2))))
          (om::x-append (om::sort-list(approx-oct first-hex new-hex1))
                                  (om::sort-list (approx-oct second-hex new-hex2))))))		  
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod! par-pc ((pcs list))
	:icon 01
 (let* ((pth-asc (om::sort-list pcs))
          (pth-uni (remove-duplicates pth-asc)) 
          (ord (om::posn-order pcs '<)) 
          (normal (normal-order pth-uni))
          (pr-dir (prime1 pth-uni)) 
          (pr (prime pth-uni)) 
          (fl1 (if (equal pr pr-dir) 
                    (position (first (transp-to-zero normal)) pr-dir) 
                    (position 0 (reverse (om::n-ord :integer (om::inv :integer pr)))))) 
          (fl2  (if (equal pr pr-dir) 0 1)) 
          (first (if (equal pr pr-dir) (first normal) (nth fl1 (reverse normal))))
          (ref  (if (equal pr pr-dir) (mapcar #'(lambda (input) (position input normal)) pth-uni)
                                               (mapcar #'(lambda (input) (position input (reverse normal))) pth-uni))))

(values (list pcs pth-asc pth-uni ord pr pr-dir normal fl1 fl2 first ref)
(format nil "~%
0 | pcs: ~A 
1 | pth-asc: ~A
2 | pth-uni: ~A
3 | ord: ~A
------------------------- 
4 | pr: ~A
5 | pr-dir: ~A
6 | normal:~A
7 | fl1: ~A
8 | fl2: ~A
9 | first: ~A
10 | ref: ~A
-------------------------
"  pcs pth-asc pth-uni ord pr pr-dir normal fl1 fl2 first ref))))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|								
(defun new-normal (par set-class)
(if (= (third (reverse par)) 0) 
    (mod12 (om::om+ (tenth par) set-class))
    (om::rotate (mod12 (om::om- (nth (tenth par) (reverse (ninth par))) set-class)) (* -1 (tenth par)))))

(defun new-pth-uni (par new-normal)
   (om::posn-match new-normal (om::last-elem par)))

(defun second-new-pth-uni (par new-normal)
   (om::posn-match 
    (if (= (third (reverse par)) 0) 
        new-normal 
       (om::rotate new-normal (* -1 (tenth par))))
        (om::last-elem par)))
		
(defun new-ord (pcs)
 (om::posn-order pcs '<))
 
(defun new-pth (par new-pth-uni)
 (om::posn-match new-pth-uni (fifth par)))

(defun get-new-pcs (par set-class)
 (let ((new-class (pc set-class)))
  (new-pth par (new-pth-uni par (new-normal par new-class))))) 
 |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





