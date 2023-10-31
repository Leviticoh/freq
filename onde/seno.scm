(define-module (onde seno))

(add-to-load-path (string-append (dirname (current-filename)) "/.."))

(use-modules (util iterator))


(define-public (onda-sinusoide frequenza freq-camp fase ampiezza)
	       (onda-sinusoide-campioni (floor (/ freq-camp frequenza)) (* fase (/ (/ freq-camp frequenza) (* 2 3.14159))) ampiezza freq-camp))

(define (onda-sinusoide-campioni lunghezza-onda n_campione ampiezza freq-camp)
  (let* ((fase (/ (* (remainder n_campione lunghezza-onda) (* 2 3.14159)) lunghezza-onda))
	 (campione (* (sin fase) ampiezza) ))
    (lambda () (cons campione (onda-sinusoide-campioni lunghezza-onda (+ n_campione 1) ampiezza freq-camp)))))
