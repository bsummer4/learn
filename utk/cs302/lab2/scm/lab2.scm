;; All artists
(define (get-all-songs filename)
  (map (lambda (data)
	 (song
	  (underscore->space (list-ref data 0))
	  (string->time (list-ref data 1))
	  (underscore->space (list-ref data 2))
	  (underscore->space (list-ref data 3))
	  (underscore->space (list-ref data 4))
	  (string->number (underscore->space (list-ref data 5)))))
       
       (map (lambda (string) (split-string string whitespace?))
	    (read-lines (open-input-file filename)))))


(define (make-structure all-songs)
  (define artist-names (apply set (map song-artist all-songs)))
  
  (define (make-artist artist-name)
    (define album-names
      (apply set
	     (map song-album
		  (filter all-songs
			  (lambda (x)
			    (string=? (song-artist x) artist-name))))))
    
    (define (make-album album-name)
      
      (define songs (sort!
		     (filter all-songs
			     (lambda (x)
			       (and
				(string=? (song-album x) album-name)
				(string=? (song-artist x) artist-name))))
		     (lambda (x y)
		       (apply < (map song-track (list x y))))))
      
      (define (make-track song)
	(track (song-title song) (song-time song) (song-track song)))
      
      (apply album (append (list album-name) (map make-track songs))))
    
    
    
    (apply artist
	   (append (list artist-name) (map make-album album-names))))
  
  
  
  (map make-artist artist-names))


(for-each display
	  (map artist->string
	       (make-structure (get-all-songs filename))))