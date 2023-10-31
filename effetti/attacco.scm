(define-module (effetti attacco))

(add-to-load-path (string-append (dirname (current-filename)) "/.."))

(use-modules (util iterator))

(define-public (attacco freq-camp durata onda)
	       (attacco-impl freq-camp durata onda 0))

(define (attacco-impl freq-camp durata onda n_campioni)
  (let* ((source (onda))
	 (tempo (/ n_campioni freq-camp))
	 (campione (if (null? (value source)) '() 
		     			      (* (value source) (if (< tempo durata) (/ tempo durata) 1)))))
    (lambda () (cons campione
		     (if (null? campione) (attacco-impl freq-camp durata onda n_campioni)
					  (attacco-impl freq-camp durata (next source) (+ n_campioni 1)))))))
