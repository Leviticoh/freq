(define-module (onde quadra))

(add-to-load-path (string-append (dirname (current-filename)) "/.."))

(use-modules (util iterator))


(define-public (onda-quadra frequenza freq-camp fase ampiezza)
	       (onda-quadra-campioni (floor (/ freq-camp frequenza)) (* fase (/ (/ freq-camp frequenza) (* 2 3.14159))) ampiezza))

(define (onda-quadra-campioni lunghezza-onda n_campione ampiezza)
  (let* ((fase (remainder n_campione lunghezza-onda))
	 (alto (> fase (quotient lunghezza-onda 2)))
	 (campione (if alto
		     ampiezza
		     (- 0 ampiezza))))
    (lambda () (cons campione (onda-quadra-campioni lunghezza-onda (+ n_campione 1) ampiezza)))))
