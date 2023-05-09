;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OMLindberg Library for OpenMusic
;;; version 1.0 - april 2023
;;; BY PAULO HENRIQUE RAPOSO - 2023


(in-package :om)

(mapc 'compile&load (list
                         (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "package" :type "lisp") 
                         (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "utils" :type "lisp")
                         (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "interpolation" :type "lisp")
                         (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "set-theory" :type "lisp")
                         (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "reconstruction" :type "lisp")
                         (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "symmetrical" :type "lisp")
						 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "chord-progressions" :type "lisp")
                         (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "harmonic-series" :type "lisp") 
                         (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "virtual-fundamental-screamer" :type "lisp") 							 						                                                
                  
					)
)

(fill-library '(

    ("INTERPOLATION" Nil Nil (omlindberg::ML-interpolation

    ) Nil)

    ("SET-THEORY" Nil Nil (omlindberg::normal-order
		                   omlindberg::prime
						   omlindberg::prime1
						   omlindberg::pc
						   omlindberg::pc?
    ) Nil)

    ("RECONSTRUCTION-OF-A-CHORD" Nil Nil (omlindberg::par
		                                  omlindberg::flatten
										  omlindberg::approx-oct
										  omlindberg::dist-classes										  
										  omlindberg::get-new
                                          omlindberg::chain-get-new
                                          omlindberg::get-new-twelve-tone										  
    ) Nil)

    ("SYMMETRICAL-CHORDS" Nil Nil (omlindberg::symmetrical-chord
								   omlindberg::new-sttch 
                                   omlindberg::new-sttch-alt
								   omlindberg::random-sttch
                                   omlindberg::all-sttchords
                                   omlindberg::all-sttchords-alt
                                   omlindberg::search-sttch                                   
                                   omlindberg::search-sttch-alt
                                   omlindberg::ml-hexachords
								   
    ) Nil)

    ("HARMONIC-REGION" Nil Nil (omlindberg::wide-scale
		                        omlindberg::synthetic-scale
    ) Nil)
	
    ("CHORD-PROGRESSIONS" Nil Nil (omlindberg::superimpose
		                           omlindberg::freeze
								   omlindberg::auto-transp
    ) Nil)	

    ("HARMONIC-SERIES" Nil Nil (omlindberg::harmonic-series
								s::ML-virtual-fundamental 
                                omlindberg::harmonic-ttch
                                omlindberg::WTVF
    ) Nil)

    ("UTILS" Nil Nil (omlindberg::chord-seq->bpf-lib
                      omlindberg::pc->mc
                      omlindberg::mc->pc
					  omlindberg::mod12
					  omlindberg::chord->intervals
					  omlindberg::intervals->chord
					  omlindberg::pc->chord
                      omlindberg::first-last-6
					  
    ) Nil)  

))

(print 
"
OMLindberg Library for OpenMusic
by Paulo Henrique Raposo - 2023

")
