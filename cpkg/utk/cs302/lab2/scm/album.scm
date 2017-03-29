(define (album name . tracks)
  (append (list name) tracks))

(define (album-name album)
  (list-ref album 0))

(define (album-tracks album)
  (list-tail album 1))


(define (album-time album)
  (let ((times (map track-time (album-tracks album))))
    (if (= (length times) 0)
	(time 0 0)
	(apply time+ times))))
  
(define (album-numtracks album)
  (length (album-tracks album)))


(define (album->string album)
  (apply string-append
	 (append
	  (list (string-append
		 "        "
		 (album-name album)
		 ": "
		 (number->string (album-numtracks album))
		 ", "
		 (time->string (album-time album))
		 "\n"))
	  (map track->string (album-tracks album)))))


(define (album->sexp album)
  (let ((name (album-name album))
	(tracks (album-tracks album)))
    (append (list 'album name) (map track->sexp tracks))))

