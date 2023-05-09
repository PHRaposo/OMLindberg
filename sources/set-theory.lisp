(in-package :omlindberg)

(defun normal-ord (list-of-pcs)
(let* ((first-normal (om::n-ord :integer list-of-pcs))
       (all-transp 
        (loop for x from 0 to 11
             for y = (mapcar #'(lambda (input1)
                      (mod (om::om+ x input1) 12)) first-normal)
             collect y)))

(loop for x from 0
      for y = (length (intersection (nth x all-transp) list-of-pcs))   
      when (= y (length list-of-pcs)) 
      return (nth x all-transp))))

(om::defmethod! normal-order ((list-of-pcs list))
 :initvals '( (2 5 7 8 10 11))
	:indoc '("pitch-classes") 
	:icon 01
	:doc "Calculates the normal-order from a list of pitch-classes retaining the original transposition."
(normal-ord list-of-pcs))

(om::defmethod! prime1 ((chord-pc list))
 :initvals '( (2 5 7 8 10 11))
	:indoc '("pitch-classes") 
	:icon 01
	:doc "Calculates the direct prime-form from a list of pitch-classes."    
 (let* ((normal (normal-ord (remove-duplicates chord-pc)))
          (normal-zero (mod12 (om::om- normal (first normal))))
          (pr (om::p-form :integer normal))
          (normal-inv (normal-ord (om::inv :integer pr)))           
         (pr-dir (mod12 (om::om- normal-inv (first normal-inv)))))
(if (equal normal-zero pr) pr pr-dir)))

(om::defmethod! prime ((chord-pc list))
 :initvals '( (2 5 7 8 10 11))
	:indoc '("pitch-classes") 
	:icon 01
	:doc "Calculates the prime-form from a list of pitch-classes."
(om::p-form :integer (remove-duplicates chord-pc)))

(defun pclass (input-str)
(cond  ((find #\A input-str :test #'equal) (om::flat (om::pc-set :integer (string-to-list (remove #\A input-str)))))
           ((find #\a input-str :test #'equal) (om::flat (om::pc-set :integer (string-to-list (remove #\a input-str)))))
           ((find #\B input-str :test #'equal) (prime1 (om::inv :integer (om::flat (om::pc-set :integer (string-to-list (remove #\B input-str)))))))
           ((find #\b input-str :test #'equal) (prime1 (om::inv :integer (om::flat (om::pc-set :integer (string-to-list (remove #\b input-str)))))))

            (t  (cond    ((equal input-str "0-1") nil)
				         ((equal input-str "1-1") (list 0))
                         ((equal input-str "11-1") (list 0 1 2 3 4 5 6 7 8 9 10))
                         ((equal input-str "12-1") (list 0 1 2 3 4 5 6 7 8 9 10 11))
                         (t (om::flat (om::pc-set :integer (string-to-list input-str) )))))))

(defun pitch-class (input-symbol)
 (let ((sym-to-str (symbol-name input-symbol)))
(pclass sym-to-str)))

(om::defmethod! pc ((set-class symbol))
 :initvals '( om::3-3b)
	:indoc '("Symbol or List" "Forms") 
	:icon 01
	:doc "Returns a list of integers."
 (pitch-class set-class))

(om::defmethod! pc ((set-class list))
 :initvals '( (om::6-27A (om::3-3a om::3-3b) (om::6-27B)))
	:indoc '("Symbol or List") 
	:icon 01
	:doc "Returns a list of integers."
(mapcar #'(lambda (input1)
 (if (listp input1) 
     (mapcar #'pitch-class input1) 
     (pitch-class input1)))
set-class))

(defun pclass? (midics)
(if (null midics) (first (string-to-list "0-1"))
(let* ((midics-pcs (mc->pc midics))
         (no-dup (remove-duplicates midics-pcs)))
(cond ((= (length no-dup) 1) (first (string-to-list "1-1")))
          ((= (length no-dup) 11) (first (string-to-list "11-1")))
          ((= (length no-dup) 12) (first (string-to-list "12-1")))
          (t (let* ((normal (normal-ord no-dup))
                      (normal-zero (mod12 (om::om- normal (first normal))))
                      (pr (prime no-dup))
                      (inv-pr1 (prime1 (om::inv :integer pr)))
                      (results 
                          (cond  ((and (not (equal pr inv-pr1)) (equal normal-zero inv-pr1)) (concatenate 'string (symbol-name (om::p-form :fn no-dup)) "b"))
                                     ((and (not (equal pr inv-pr1)) (equal normal-zero pr)) (concatenate 'string (symbol-name (om::p-form :fn no-dup)) "a"))
                            (t (symbol-name (om::p-form :fn no-dup))))))
            (first (string-to-list results))))))))

(defun pclass-integers? (pcs)
(if (null pcs)  (first (string-to-list "0-1"))
(let* ((no-dup (remove-duplicates pcs)))
(cond ((and (= (length no-dup) 1) (numberp nodup)) (first (string-to-list "1-1")))
          ((= (length no-dup) 11) (first (string-to-list "11-1")))
          ((= (length no-dup) 12) (first (string-to-list "12-1")))
          (t (let* ((normal (normal-ord no-dup))
                      (normal-zero (mod12 (om::om- normal (first normal))))
                      (pr (prime no-dup))
                      (inv-pr1 (prime1 (om::inv :integer pr)))
                      (results 
                          (cond  ((and (not (equal pr inv-pr1)) (equal normal-zero inv-pr1)) (concatenate 'string (symbol-name (om::sub-power :fn (length no-dup) (length no-dup) no-dup)) "b"))
                                     ((and (not (equal pr inv-pr1)) (equal normal-zero pr)) (concatenate 'string (symbol-name (om::sub-power :fn (length no-dup) (length no-dup) no-dup)) "a"))
                            (t (symbol-name (om::sub-power :fn (length no-dup) (length no-dup) no-dup))))))
            (first (string-to-list results))))))))

(om::defmethod! pc? ((midics list) &optional (forms :a-b))
 :initvals '( (6000 6300 6400) :a-b)
	:indoc '("List of midics" "a-b or prime") 
	:icon 01
    :menuins '( (1 (("a-b" :a-b) ("prime" :prime))))
	:doc "Returns the name of a set-class in Forte notation from a list of midics."
(if (equal forms :prime)
    (if (list-of-listp midics)
	    (mapcar #'(lambda (input) (om::p-form :fn (mc->pc input))) midics)
		(om::p-form :fn (mc->pc midics)))
(if (list-of-listp midics)
     (mapcar #'pclass? midics)
     (pclass? midics))))

