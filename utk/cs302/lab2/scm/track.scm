(define (track name time number)
  (list name time number))

(define (track-name track)
  (list-ref track 0))

(define (track-time track)
  (list-ref track 1))

(define (track-number track)
  (list-ref track 2))


(define (track->string track)
  (apply string-append
	 (append
	  (list (string-append
		 "                "
		 (number->string (track-number track))
		 ". "
		 (track-name track)
		 ": "
		 (time->string (track-time track))
		 "\n")))))

(define (track->sexp track)
  (let ((name (track-name track))
	(time (track-time track))
	(number (track-number track)))
    (list 'track name (time->sexp time) number)))