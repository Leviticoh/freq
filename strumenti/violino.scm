(define-module (strumenti violino))

(add-to-load-path (string-append (dirname (current-filename)) "/../lib"))
(add-to-load-path (string-append (dirname (current-filename)) "/.."))

(use-modules (guile-iterators iterator) (effetti smorzamento) (effetti attacco) (onde seno))


(define-public (violino tau freq-camp frequenza ampiezza)
  (attacco freq-camp 0.5 (iter-zip-with +
		  (iter-zip-with +
				 (smorzamento (/ tau 2) freq-camp (onda-sinusoide (* frequenza 2) freq-camp 0 ampiezza))
				 (smorzamento (/ tau 4) freq-camp (onda-sinusoide (* frequenza 4) freq-camp 0 ampiezza)))
		  (smorzamento tau freq-camp (onda-sinusoide frequenza freq-camp 0 ampiezza)))))
