(define (time mins secs)
  (cons mins secs))

(define (time-mins time)
  (car time))

(define (time-secs time)
  (cdr time))


(define (string->time string)
  (let ((split (split-string string
			     (lambda (c) (char=? c #\:)))))
    (time (string->number (list-ref split 0))
	  (string->number (list-ref split 1)))))

(define (time->sexp time)
  (list 'time (time-mins time) (time-secs time)))

(define (time->string time)
  (string-append
   (number->string (time-mins time))
   ":"
   (pad (number->string (time-secs time)) 2 #\0)))


(define (time+ time1 . times)
  (define (adder time1 time2)
    (let ((seconds (+ (time-secs time1) (time-secs time2)))
	  (minutes (+ (time-mins time1) (time-mins time2))))
      (if (< seconds 60)
	  (time minutes seconds)
	  (time (+ 1 minutes) (- seconds 60)))))
  
  (cond ((null? times) time1)
	((= 1 (length times)) (adder time1 (list-ref times 0)))
	(else (adder time1 (apply time+ times)))))