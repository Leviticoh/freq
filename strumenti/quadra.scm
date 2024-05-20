(define-module (strumenti quadra))

(add-to-load-path (string-append (dirname (current-filename)) "/../lib"))
(add-to-load-path (string-append (dirname (current-filename)) "/.."))

(use-modules (guile-iterators iterator) (onde quadra))


(define-public (quadra freq-camp frequenza ampiezza)
	       (onda-quadra frequenza freq-camp 0 ampiezza))
