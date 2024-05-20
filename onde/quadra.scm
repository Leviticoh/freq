(define-module (onde quadra))

(add-to-load-path (string-append (dirname (current-filename)) "/../lib"))
(add-to-load-path (string-append (dirname (current-filename)) "/.."))

(use-modules (guile-iterators iterator))


(define-public (onda-quadra frequenza freq-camp fase ampiezza)
	       (onda-quadra-campioni (floor (/ freq-camp frequenza)) (* fase (/ (/ freq-camp frequenza) (* 2 3.14159))) ampiezza))


(define (onda-quadra-campioni lunghezza-onda n_campione ampiezza)
  (iter-map (lambda (n_campioni)
	      (let* ((fase (remainder n_campioni lunghezza-onda))
		     (alto (> fase (quotient lunghezza-onda 2))))
		(if alto ampiezza (- 0 ampiezza))))
	    (iter-scan + n_campione
		       (repeat 1))))
