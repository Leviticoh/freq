(define-module (effetti smorzamento))

(add-to-load-path (string-append (dirname (current-filename)) "/.."))

(use-modules (util iterator))


(define-public (smorzamento tau freq-camp onda)
	       (iter-map cdr
			 (iter-scan (lambda (stato campione)
				      (let* ((n_campioni (car stato))
					     (tempo (/ n_campioni freq-camp))
					     (risultato (/ campione (exp (/ tempo tau)))))
					(cons (+ n_campioni 1) risultato)))
				    '(0 . 0)
				    onda)))
