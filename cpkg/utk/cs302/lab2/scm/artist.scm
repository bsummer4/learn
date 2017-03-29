(define (artist name . albums)
  (append (list name) albums))

(define (artist-name artist)
  (list-ref artist 0))

(define (artist-albums artist)
  (list-tail artist 1))


(define (artist-numtracks artist)
  (apply + (map album-numtracks (artist-albums artist))))

(define (artist-time artist)
  (let ((times (map album-time (artist-albums artist))))
    (if (= (length times) 0)
	(time 0 0)
	(apply time+ times))))


(define (artist->sexp artist)
  (let ((name (artist-name artist))
	(albums (artist-albums artist)))
    (append (list 'artist name) (map album->sexp albums))))

(define (artist->string artist)
  (apply string-append
	 (append
	  (list (string-append
	   (artist-name artist)
	   ": "
	   (number->string (artist-numtracks artist))
	   ", "
	   (time->string (artist-time artist))
	   "\n"))
	  (map album->string (artist-albums artist)))))
