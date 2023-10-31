

(define-module (util iterator))
(use-modules (ice-9 textual-ports))

(define-public value car)
(define-public next cdr)

(define-public (lines port)
	       (lambda () (let ((line (get-line port)))
			    (cons (if (eof-object? line) '() line)
				  (lines port)))))

(define-public (iterate l)
	       (lambda () (cons (if (null? l) '() (car l))
				(iterate (if (null? l) '() (cdr l))))))

(define-public (repeat e)
	       (lambda () (cons e (repeat e))))

(define-public (iter-map fun iter)
	       (let ((source (iter)))
		 (lambda () (cons (if (null? (value source)) '() (fun (value source)))
				  (iter-map fun (next source))))))

(define-public (iter-scan fun state iter)
	       (let* ((source (iter))
		      (result (if (null? (value source)) '() (fun state (value source)))))
		 (lambda () (cons result
				  (iter-scan fun result (next source))))))

(define-public (iter-zip-with fun iter-a iter-b)
	       (let ((source-a (iter-a))
		     (source-b (iter-b)))
		 (lambda () (cons (if (or (null? (value source-a))
					  (null? (value source-b)))
				    '()
				    (fun (value source-a) (value source-b)))
				  (iter-zip-with fun
						 (next source-a)
						 (next source-b))))))

(define-public (iter-sum iters)
	       (if (null? iters)
		 (repeat 0)
		 (iter-zip-with + (car iters) (iter-sum (cdr iters)))))

(define-public (iter-take n iter)
	       (let ((source (iter)))
		 (lambda () (cons (if (or (null? (value source))
					  (<= n 0))
				    '() (value source))
				  (iter-take (- n 1) (next source))))))

(define-public (iter-concat iter-iter)
	       (let ((source (iter-iter)))
		 (iter-concat-impl (next source) (value source))))

(define (iter-concat-impl iter-iter iter)
  (let* ((src (iter))
	 (new-iter (if (null? (value src)) (iter-iter) '(() . ())))
	 (source   (if (and (null? (value src))
			    (not (null? (value new-iter))))
		     ((value new-iter)) src)))
    (lambda () (cons (value source)
		     (cond ((and (null? (value src))
				 (null? (value new-iter)))
			    (iter-concat-impl iter-iter iter))
			   ((and (null? (value src))
				 (not (null? (value new-iter))))
			    (iter-concat-impl (next new-iter) (next source)))
			   ((not (null? (value src)))
			    (iter-concat-impl iter-iter (next source))))))))

(define-public (iter-for-each fun iter)
	       (let* ((source (iter))
		      (n (null? (value source))))
		 (if (not n) (fun (value source)))
		 (if (not n) (iter-for-each fun (next source)))))
