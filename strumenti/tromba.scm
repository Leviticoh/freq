(define-module (strumenti tromba))

(add-to-load-path (string-append (dirname (current-filename)) "/.."))

(use-modules (util iterator) (effetti smorzamento) (effetti attacco) (onde seno))


(define-public (tromba freq-camp frequenza ampiezza)
  (attacco freq-camp 0.1 (iter-sum (list
				   (onda-sinusoide (* frequenza 4) freq-camp 0 (/ ampiezza 3))
				   (onda-sinusoide (* frequenza 3) freq-camp 0 (/ ampiezza 1))
				   (onda-sinusoide (* frequenza 2) freq-camp 0 (/ ampiezza 8))
				   (onda-sinusoide frequenza freq-camp 0 ampiezza)))))
