(in-package :omlindberg)

(defvar *omlindberg-path* (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "Symmetrical-Chords"))))

(defun create-sttch (lowest-note forte-name form permutation axis-options)
(let* ((pc-set (om::pc-set :integer forte-name)) 
       (inversion (om::inv :integer pc-set))
       (permut (cond ((equal form "A") (om::posn-match pc-set permutation))
                      ((equal form "B") (om::posn-match (reverse inversion) permutation))
                     (t (write 'nil))))     
       (all-forms (mapcar #' (lambda (input1) 
                   (ascending-chord input1)) (all-symmetric-complements permut)))
      (selected-form (loop for x from 0 to (- (length all-forms) 1)
                                       if (evenp x)
                                         collect (nth x all-forms) into selected                     
                                       else
                                         collect (nth x all-forms) into unselected
                                       finally (return selected))))
(om::om+ lowest-note (om::om- (nth axis-options selected-form) (first (nth axis-options selected-form))))))

(defun create-sttch-alt (lowest-note forte-name permutation axis-options)
(let* ((pc-set (om::pc-set :integer forte-name))        
       (permut (om::posn-match pc-set permutation)) 
       (all-forms (mapcar #' (lambda (input1) 
                   (ascending-chord input1)) (all-symmetric-complements-alt permut))))
(nth axis-options (mapcar #'(lambda (input1) 
                   (om::om+ lowest-note (om::om- input1 (first input1)))) all-forms))))

(defun string-to-list (str) ; Solution by Banjocat - stackoverflow.com
        (if (not (streamp str))
           (string-to-list (make-string-input-stream str))
           (if (listen str)
               (cons (read str) (string-to-list str))
               nil)))

(defun read-text-file (string-fn) 
(let ((my-stream (open (merge-pathnames *omlindberg-path* (parse-namestring (concatenate 'string string-fn ".txt"))))))
(om::flat-once (omlindberg::string-to-list my-stream))))  

(om::defmethod! all-sttchords ((forte-name om::t))
        :initvals '(om::6-2)
	:indoc '("pc-set")
        :menuins '( (0 (("6-2" 'om::6-2) ("6-5" 'om::6-5) ("6-9" 'om::6-9) ("6-15" 'om::6-15) ("6-16" 'om::6-16) ("6-18" 'om::6-18) ("6-21" 'om::6-21) ("6-22" 'om::6-22) ("6-27" 'om::6-27) ("6-30" 'om::6-30) ("6-31" 'om::6-31) ("6-33" 'om::6-33) ("6-34" 'om::6-34)))) 
	:icon 01
	:doc "Generates all possible symmetrical twelve-tone chords by choosing one of the 13 type of given hexachords."
(read-text-file (symbol-name forte-name)))
;(all-sttch forte-name))

;;;;;; CALCULATIONS FOR M.LINDBERG HEXACHORDS ;;;;;;
    ;;;;;; .TXT FILES IN "SOURCES/SYMMETRICAL-CHORDS" FOLDER ;;;;;;


(defun all-sttch (om::forte-name)
(let* ((pc-set (om::pc-set :integer om::forte-name))        
       (all-forms (all-symmetric-complements pc-set))
       (permut (cond ((equal om::forte-name 'om::6-30)                   
                      (om::first-n (om::permutations pc-set)
                           (/ 720 (/ (length all-forms) 2))))  
                 (t (om::permutations pc-set))))
        (symm-comps (om::flat-once (mapcar #' (lambda (input1) 
                    (all-symmetric-complements input1)) permut)))      
        (all-chords (mapcar #' (lambda (input1) 
                   (ascending-chord input1)) symm-comps)))
(mapcar #' (lambda (input1)
           (om::om+ 3600 (om::om- input1 (first input1)))) all-chords)))


;;;;; CALCULATIONS FOR ALTERNATIVE HEXACHORDS ;;;;;;
    ;;;;;; .TXT FILES IN "SOURCES" FOLDER ;;;;;;

(defun all-sttch-alt (om::forte-name)
(let* ((pc-set (om::pc-set :integer om::forte-name))         
       (all-forms (all-symmetric-complements pc-set))
       (permut (cond ((or (equal om::forte-name 'om::6-7)
                          (equal om::forte-name 'om::6-20)
                          (equal om::forte-name 'om::6-35))                    
                 (om::first-n (om::permutations pc-set)
                     (/ 720 (/ (length all-forms) 2))))
         (t (om::permutations pc-set))))
        (symm-comps (om::flat-once (mapcar #' (lambda (input1) 
                    (all-symmetric-complements-alt input1)) permut)))      
        (all-chords (mapcar #' (lambda (input1) 
                   (ascending-chord input1)) symm-comps)))
(mapcar #' (lambda (input1)
           (om::om+ 3600 (om::om- input1 (first input1)))) all-chords)))
		   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun all-symmetric-complements (pc-set)
(let* ((inversion (om::inv :integer pc-set))
      (complement (set-complement-pcs pc-set)))
      
   (loop for x from 0 to 11 

      if (= (length pc-set) 
            (length (intersection (mapcar #'(lambda (input1)
                                  (mod (om::om+ input1 x) 12)) inversion) 
                    complement)))
         collect (om::x-append
                 (list (om::x-append 
                  pc-set
                 (reverse (mapcar #'(lambda (input1)
                 (mod (om::om+ input1 x) 12)) inversion))))
                 (list (om::x-append 
                 (reverse (mapcar #'(lambda (input1)
                 (mod (om::om+ input1 x) 12)) inversion))
                 pc-set))) into symmetric-hexachords 
      else
         collect x into false-results
      finally (return (om::flat-once symmetric-hexachords)))))

(defun all-symmetric-complements-alt (pc-set)
(let* ((inversion (om::inv :integer pc-set))
      (complement (set-complement-pcs pc-set)))
      
   (loop for x from 0 to 11 

      if (= (length pc-set) 
            (length (intersection (mapcar #'(lambda (input1)
                                  (mod (om::om+ input1 x) 12)) inversion) 
                    complement)))
         collect (list (om::x-append 
                  pc-set
                 (reverse (mapcar #'(lambda (input1)
                 (mod (om::om+ input1 x) 12)) inversion))))
                 into symmetric-hexachords 
      else
         collect x into false-results
      finally (return (om::flat-once symmetric-hexachords)))))

(om::defmethod! new-sttch ((lowest-note number) (forte-name om::t) (form string) (permutation list) &optional (axis-options 0))
       :initvals '(4800 om::6-2 "A" (0 1 2 3 4 5) 0)
       :indoc '("lowest-note" "pc-set" "AB or BA" "list-of-positions" "axis options") 
       :icon 01
       :menuins '( (1 (("6-2" 'om::6-2) ("6-5" 'om::6-5) ("6-9" 'om::6-9) ("6-15" 'om::6-15) ("6-16" 'om::6-16) ("6-18" 'om::6-18) ("6-21" 'om::6-21) ("6-22" 'om::6-22) ("6-27" 'om::6-27) ("6-30" 'om::6-30) ("6-31" 'om::6-31) ("6-33" 'om::6-33) ("6-34" 'om::6-34))) (2 (("A" "A") ("B" "B"))))
	:doc "Creates a symmetric twelve-tone chord by choosing the lowest note <in midicents>, one of the 13 types of given hexachords <forte-name>, prime form 'A' or inversion 'B' and a permutation <list>. The optional argument,  <axis-options> 0 or 1, is only for the hexachord 6-30, that is also a mode of limited transposition."
(create-sttch lowest-note forte-name form permutation axis-options))

(om::defmethod! new-sttch-alt ((lowest-note number) (forte-name om::t) (permutation list) &optional (axis-options 0))
       :initvals '(4800 om::6-2 (0 1 2 3 4 5) 0)
       :indoc '("lowest-note" "pc-set" "list-of-positions" "axis options") 
       :icon 01
       :menuins '( (1 (("6-1" 'om::6-1) ("6-7" 'om::6-7) ("6-8" 'om::6-8) ("6-20" 'om::6-20) ("6-32" 'om::6-32) ("6-35" 'om::6-35)))) 
	:doc "Creates a symmetric twelve-tone chord by choosing the lowest note <in midicents>, one of the given hexachords <forte-name> and a permutation <list>. The optional argument <axis-options> is for the modes of limited transposition (6-7, 6-20 and 6-35)."
(create-sttch-alt lowest-note forte-name permutation axis-options))

(om::defmethod! all-sttchords-alt ((forte-name om::t))
        :initvals '(om::6-1)
	:indoc '("pc-set")
        :menuins '( (0 (("6-1" 'om::6-1) ("6-7" 'om::6-7) ("6-8" 'om::6-8) ("6-20" 'om::6-20) ("6-32" 'om::6-32) ("6-35" 'om::6-35))))
	:icon 01
	:doc "Generates all possible symmetrical twelve-tone chords by choosing one of the 6 types of given hexachords."
(read-text-file (symbol-name forte-name)))
;(all-sttch-alt forte-name))

(defun width-dist (chord1 chord2)
 (let* ((width1 (- (om::last-elem chord1) (first chord1)))
        (width2 (- (om::last-elem chord2) (first chord2))))
  (om::om-abs (- width1 width2)))) 

(defun width-filter (comb-chords model-chord)
 (let* ((width-distances (mapcar #' (lambda (input1)
                          (width-dist input1 model-chord)) comb-chords)))
(om::posn-match comb-chords (tristan-positions width-distances (om::list-min width-distances)))))

(defun axis-transp (chord1 chord2)
 (let* ((intervals-down (om::x->dx (reverse (om::first-n chord1 7))))
        (intervals-up (om::x->dx (om::last-n chord1 6)))
        (axis1 (/ (- (seventh chord1) (sixth chord1)) 2))
        (axis2 (+ (first chord2) (/ (- (om::last-elem chord2) (first chord2)) 2)))
        (new-axis (* 100 (om::om-round (+ axis1 axis2) 0 100))))
 (om::sort-list (om::x-append (cdr (om::dx->x new-axis intervals-down)) (om::dx->x new-axis intervals-up)))))

(defun axis-transposition (chord1 model-chord)
 (mapcar #' (lambda (input1)
  (axis-transp input1 model-chord)) chord1))

;Thanks to Mikhail Malt! ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun euclides (chord1 chord2)
 (apply '+ (om::om-abs (om::om- chord1 chord2)))) 

(defun min-dist-chord (comb-chords model-chord)
 (let* ((abs-distances (mapcar #' (lambda (input1)
                       (euclides input1 model-chord)) comb-chords))
        (chord-position (position (om::list-min abs-distances) abs-distances)))
(nth chord-position comb-chords)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun s-ttch (forte-name model-chord)
(let* ((combinatorial-chords (omlindberg::read-text-file forte-name))
       (filtered-chords (width-filter combinatorial-chords model-chord))
       (transp-chords (axis-transposition filtered-chords model-chord)))
(min-dist-chord transp-chords model-chord)))

(defun s-ttch-alt (forte-name model-chord)
(let* ((combinatorial-chords (omlindberg::read-text-file forte-name))
       (filtered-chords (width-filter combinatorial-chords model-chord))
       (transp-chords (axis-transposition filtered-chords model-chord)))
(min-dist-chord transp-chords model-chord)))

(om::defmethod! search-sttch ((forte-name string) (model-chord list))
        :initvals '("6-2" (4800 5200 5500 5700 5900 6200 6400 6600 6700 7100 7200 7600))
	:indoc '("pc-set" "list or list-of-lists of midicents")
        :menuins '( (0 (("6-2" '"6-2") ("6-5" '"6-5") ("6-9" '"6-9") ("6-15" '"6-15") ("6-16" '"6-16") ("6-18" '"6-18") ("6-21" '"6-21") ("6-22" '"6-22") ("6-27" '"6-27") ("6-30" '"6-30") ("6-31" '"6-31") ("6-33" '"6-33") ("6-34" '"6-34"))))
	:icon 01
	:doc "Returns the closest symmetrical twelve-tone chord from <model-chord>. The optional argument selects a chord 'list' or a list of chords 'lists'."
(if (list-of-listp model-chord)
     (mapcar #'(lambda (input)
       (s-ttch forte-name input)) model-chord)
 (if (listp model-chord)
      (s-ttch forte-name model-chord)
 (error "The input argument should be a list or a list of lists of midicents."))))

(om::defmethod! search-sttch-alt ((forte-name string) (model-chord list))
        :initvals '("6-1" (4800 5200 5500 5700 5900 6200 6400 6600 6700 7100 7200 7600) "list")
	:indoc '("pc-set" "list or list of lists of midicents")
        :menuins '( (0 (("6-1" '"6-1") ("6-7" '"6-7") ("6-8" '"6-8") ("6-20" '"6-20") ("6-32" '"6-32") ("6-35" '"6-35"))))  
	:icon 01
	:doc "Returns the closest symmetrical twelve-tone chord from <model-chord>."
(if (list-of-listp model-chord)
     (mapcar #'(lambda (input)
       (s-ttch-alt forte-name input)) model-chord)
 (if (listp model-chord)
      (s-ttch-alt forte-name model-chord)
 (error "The input argument should be a list or a list of lists of midicents."))))

(defun get-pc-set (chord)
(let* ((first-6 (om::first-n chord 6))
	   (last-6 (om::last-n chord 6)))
 (om::x-append (pc? first-6) (pc? last-6))))

(om::defmethod! ml-hexachords ((midics list)) 
  :initvals '( (4000 4700 5400 5800 6100 6500 6800 7200 7500 7900 8600 9300)) 
  :indoc '("list or list-of-lists") 
  :icon 01
  :doc "Returns the forte-name of each hexachord of a twelve-tone chord. If the optional argument is set to 'lists', this function can be apllied to a list fo chords."
 (cond ((list-of-listp midics)
	    (mapcar #'get-pc-set midics))
  (t (listp midics)
      (get-pc-set midics))))

(om::defmethod! wide-scale ((chord list) (low number) (high number))
 :initvals '((5100 5500 5700 5800 6100 6400 6500 6800 7100 7200 7400 7800) 2100 10800)
	:indoc '("midicents" "lowest note" "highest note") 
	:icon 01
	:doc "Returns a wide-scale constructed by multiple transpositions of a twelve tone chord."
 (let ((transpositions
           (remove-duplicates
            (om::sort-list
            (om::flat          
             (mapcar #'(lambda (input)
              (append (om::om+ '(0 2400 4800 7200) input)
                            (om::om- input '(0 2400 4800 7200))))
             chord))))))
  (loop for x in transpositions
   if (and (>= x low)
               (<= x high))
    collect x)))

(defun random-sttchord (n accumul low high)
   (let ((random-chord (new-sttch
                                   (om::nth-random (om::arithm-ser low high 100))
                                   (om::nth-random '(om::|6-2| om::|6-5| om::|6-9| om::|6-15| om::|6-16| om::|6-18| om::|6-21| om::|6-22| om::|6-27| om::|6-30| om::|6-31| om::|6-33| om::|6-34|))
                                  (om::nth-random '("A" "B"))
                                  (om::permut-random '(0 1 2 3 4 5))
                                   0)))
 (if (equal n (length accumul))
     (write accumul)
  (if (and (>= (first random-chord) low)
              (<= (om::last-elem random-chord) high))
      (random-sttchord n (om::x-append (remove nil accumul) 
                                                             (list random-chord))
                                  low
                                  high)
    (random-sttchord n accumul low high)))))

(om::defmethod! random-sttch ((n integer) (low integer) (high integer))
        :initvals '( 8 2100 10800)
	:indoc '("n-chords" "lowest-note" "highest-note") 
	:icon 01
	:doc "Generates 'n' random symmetrical twelve-tone chords. The ambitus of all chords is defined by low (lowest note) and high (highest note). 
                 One of the 15 Lindberg's hexachords is randomly selected for each chord."
(random-sttchord n nil low high))

(defvar *open-fifths*  (mapcar #'(lambda (input)
                                    (om::arithm-ser input 10800 700)) 
                                  '(2100 2200 2300 2400 2500 2600 2700)))

(defun synth-scale (fifths)     
 (om::sort-list
  (om::flat
   (mapcar #'(lambda (input)
   (om::dx->x input '(200 100 200 100))) fifths))))

(defun best-fifths (chord)
 "Calculates the Euclidean distance between all possible chords by fifths and the input chord, selectiing the closest one."
 (let* ((common-notes (mapcar #'(lambda (input)
                                     (length (om::x-intersect chord input))) *open-fifths*))
          (position-max (position (om::list-max common-notes)
                                                common-notes)))
 (nth position-max *open-fifths*)))

(om::defmethod! synthetic-scale ((chord list))
        :initvals '( (4700 5100 5200 5400 5700 5800 6100 6200 6500 6700 6800 7200))
	:indoc '("chord-midics") 
	:icon 01
	:doc "Generates a synthetic scale [2 1 2 1 1] from a twelve-tone chord."
(synth-scale (best-fifths chord)))


(defun symm-chord (chord)
 (let* ((chord-pcs (om::sort-list (mcs-to-pcs chord)))
          (inversions (mod12 
                             (mapcar #'(lambda (input1) 
                              (om::dx->x input1 (om::om/ (om::om* -1 (om::x->dx chord)) 100))) '(0 1 2 3 4 5 6 7 8 9 10 11))))
          (remove-ct (remove-if-not #'(lambda (input1) (= (length (om::x-intersect chord-pcs input1)) 0)) inversions))
          (append-new-notes (mapcar #'(lambda (input1) (om::x-append (mc->pc chord) (reverse input1))) remove-ct))
          (results (mapcar #'(lambda (input1)
                        (pc->chord input1 (first (om::om// (om::sort-list chord) 1200)))) append-new-notes)))
(if (= 1 (length results)) (om::flat-once results) results)))

(om::defmethod! symmetrical-chord ((chord list))
        :initvals '( (6600 6700 6900 7200 7300 7500))
	:indoc '("chord-midics") 
	:icon 01
	:doc "Generates all possible symmetrcial chords without common notes "
(symm-chord chord))
