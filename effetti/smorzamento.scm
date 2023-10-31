(define-module (effetti smorzamento))

(add-to-load-path (string-append (dirname (current-filename)) "/.."))

(use-modules (util iterator))

(define-public (smorzamento tau freq-camp onda)
	       (smorzamento-impl tau freq-camp onda 0))

(define (smorzamento-impl tau freq-camp onda n_campioni)
  (let* ((source (onda))
	 (tempo (/ n_campioni freq-camp))
	 (campione (if (null? (value source)) '() 
		     			      (/ (value source) (exp (/ tempo tau))))))
    (lambda () (cons campione
		     (if (null? campione) (smorzamento-impl tau freq-camp onda n_campioni)
					  (smorzamento-impl tau freq-camp (next source) (+ n_campioni 1)))))))
