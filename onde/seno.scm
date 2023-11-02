(define-module (onde seno))

(add-to-load-path (string-append (dirname (current-filename)) "/.."))

(use-modules (util iterator))


(define-public (onda-sinusoide frequenza freq-camp fase ampiezza)
	       (onda-sinusoide-campioni (floor (/ freq-camp frequenza)) (* fase (/ (/ freq-camp frequenza) (* 2 3.14159))) ampiezza freq-camp))

(define (onda-sinusoide-campioni lunghezza-onda n_campione ampiezza freq-camp)
  (iter-map (lambda (n_campioni)
	      (let* ((fase (/ (* (remainder n_campioni lunghezza-onda)
				 (* 2 3.14159))
			      lunghezza-onda)))
		(* (sin fase) ampiezza)))
	    (iter-scan + n_campione
		       (repeat 1))))
