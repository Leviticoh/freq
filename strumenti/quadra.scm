(define-module (strumenti quadra))

(add-to-load-path (string-append (dirname (current-filename)) "/.."))

(use-modules (util iterator) (onde quadra))


(define-public (quadra freq-camp frequenza ampiezza)
	       (onda-quadra frequenza freq-camp 0 ampiezza))
