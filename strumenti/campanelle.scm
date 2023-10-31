(define-module (strumenti campanelle))

(add-to-load-path (string-append (dirname (current-filename)) "/.."))

(use-modules (util iterator) (effetti smorzamento) (onde seno))


(define-public (campanelle tau freq-camp frequenza ampiezza)
  (iter-zip-with +
		  (iter-zip-with +
				 (smorzamento (/ tau 2) freq-camp (onda-sinusoide (* frequenza 2) freq-camp 0 ampiezza))
				 (smorzamento (/ tau 4) freq-camp (onda-sinusoide (* frequenza 4) freq-camp 0 ampiezza)))
		  (smorzamento tau freq-camp (onda-sinusoide frequenza freq-camp 0 ampiezza))))
