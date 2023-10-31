#!/usr/bin/env -S guile -s
!#
(add-to-load-path (dirname (current-filename)))
(use-modules (ice-9 pretty-print) (ice-9 textual-ports) (ice-9 binary-ports)
	     (rnrs bytevectors)
	     (util iterator)
	     (onde seno)
	     (effetti smorzamento) (effetti attacco)
	     (strumenti campanelle) (strumenti violino) (strumenti tromba))


(define frequenze
  '(261.63
    277.18
    293.66
    311.13
    329.63
    349.23
    369.99
    392.00
    415.30
    440.00
    466.16
    493.88))

(define (interpreta-nota nome modificatore)
  (let* ((nota (cond ((string=? nome "do") 0)
		     ((string=? nome "re") 2)
		     ((string=? nome "mi") 4)
		     ((string=? nome "fa") 5)
		     ((string=? nome "sol") 7)
		     ((string=? nome "la") 9)
		     ((string=? nome "si") 11)))
	 (indice (cond ((string=? modificatore "-") nota)
		       ((string=? modificatore "b") (remainder (+ nota 11) 12))
		       ((string=? modificatore "#") (remainder (+ nota 01) 12)))))
    (list-ref frequenze indice)))

(define (interpreta-tono riga)
  (let* ((parole (string-split riga #\space))
	 (nota (string-split (car parole) #\_))
	 (ottava (string->number (cadr parole)))
	 (durata (string->number (cadddr parole)))
	 (ampiezza (string->number (caddr parole)))
	 (tono (* (interpreta-nota (car nota) (cadr nota)) (expt 2 (- ottava 4)))))
    (list tono durata ampiezza)))

(define (put-sample sample)
  (let ((buf (make-bytevector 2))
	(out (cond ((> sample 32767) 32767)
		   ((< sample -32767) -32767)
		   ((and (>= sample -32767)
			 (<= sample 32767)) sample))))
    (bytevector-s16-set! buf 0 (inexact->exact (ceiling out)) (endianness little))
    (put-bytevector (current-output-port) buf)))

(define (taglio freq-camp frequenza tempo tono)
  (let* ((fine (* tempo freq-camp))
	 (inizio (- fine (/ (* freq-camp 5)) frequenza)))
    (iter-map cdr (iter-scan (lambda (stato onda)
			       (let* ((indice (car stato)))
				 (if (> indice inizio)
				   (cons (+ indice 1) (* onda (/ (- fine indice) (- fine inizio))))
				   (cons (+ indice 1) onda))))
			     '(0 . 0)
			     (iter-take (* freq-camp tempo) tono)))))

(define (onnda desc)
  (taglio 48000 (car desc) (cadr desc) (violino 0.1 48000 (car desc) (caddr desc))))


(iter-for-each put-sample
	       (iter-concat (iter-map onnda
				      (iter-map interpreta-tono
						(lines (current-input-port))))))
