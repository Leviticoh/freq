(define-module (effetti attacco))

(add-to-load-path (string-append (dirname (current-filename)) "/.."))

(use-modules (util iterator))


(define-public (attacco freq-camp durata onda)
  (iter-map cdr
	    (iter-scan (lambda (stato campione)
			 (let* ((n_campioni (car stato))
				(tempo (/ n_campioni freq-camp))
				(risultato (* campione (if (< tempo durata)
							 (/ tempo durata)
							 1))))
			   (cons (+ n_campioni 1) risultato)))
		       '(0 . 0)
		       onda)))
