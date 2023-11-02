

(define-module (util iterator))
(use-modules (ice-9 textual-ports))

(define-public value car)
(define-public next cdr)

(define-public (generate iter)
	       (let* ((funct (cdr iter))
		      (state (car iter))
		      (result (funct state))
		      (element (car result))
		      (next-state (cdr result)))
		 (cons element (cons next-state funct))))


(define-public (lines port)
	       (cons port (lambda (p)
			    (let ((line (get-line p)))
			      (cons (if (eof-object? line) '() line)
				    p)))))


(define (iterate-impl a)
  (if (null? a)
    '(() . ())
    a))

(define-public (iterate l)
	       (cons l iterate-impl))


(define-public (repeat e)
	       (cons e (lambda (a) (cons a a))))


(define (iter-map-impl a)
  (let ((source (generate (car a)))
	(fun (cdr a)))
    (if (null? (value source))
      (cons '() (next source))
      (cons (fun (value source)) (cons (next source) fun)))))

(define-public (iter-map fun iter)
	       (cons (cons iter fun) iter-map-impl))


(define (iter-scan-impl a)
  (let* ((source (generate (cadr a)))
	 (fun (cddr a))
	 (result (if (null? (value source))
		   '()
		   (fun (car a) (value source)))))
    (cons result (cons result (cons (next source) fun)))))

(define-public (iter-scan fun state iter)
	       (cons (cons state (cons iter fun))
		     iter-scan-impl))


(define (iter-zip-with-impl a)
  (let ((source-a (generate (cadr a)))
	(source-b (generate (cddr a)))
	(fun (car a)))
    (cons (if (or (null? (value source-a))
		  (null? (value source-b)))
	    '()
	    (fun (value source-a) (value source-b)))
	  (cons fun (cons (next source-a) (next source-b))))))

(define-public (iter-zip-with fun iter-a iter-b)
	       (cons (cons fun (cons iter-a iter-b))
		     iter-zip-with-impl))


(define-public (iter-sum iters)
	       (if (null? iters)
		 (repeat 0)
		 (iter-zip-with + (car iters) (iter-sum (cdr iters)))))


(define (iter-take-impl a)
  (let ((source (generate (cdr a))))
    (cons (if (or (null? (value source))
		  (<= (car a) 0))
	    '()
	    (value source))
	  (cons (- (car a) 1) (next source)))))

(define-public (iter-take n iter)
	       (cons (cons n iter)
		     iter-take-impl))

(define (iter-concat-impl iter iter-iter)
  (let ((source (generate iter)))
    (if (null? (value source))
      (let ((nextsource (generate iter-iter)))
	(if (null? (value nextsource))
	  (cons '() (cons iter iter-iter))
	  (iter-concat-impl (value nextsource) (next nextsource))))
      (cons (value source) (cons (next source) iter-iter)))))

(define-public (iter-concat iter-iter)
	       (cons (cons (repeat '()) iter-iter)
		     (lambda (a) (iter-concat-impl (car a) (cdr a)))))

(define-public (iter-for-each fun iter)
	       (let* ((source (generate iter))
		      (n (null? (value source))))
		 (if (not n) (fun (value source)))
		 (if (not n) (iter-for-each fun (next source)))))
