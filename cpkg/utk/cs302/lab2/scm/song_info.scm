(define (song title time artist album genre track)
  (list title time artist album genre track))
  
(define (song-title song)
  (list-ref song 0))
(define (song-time song)
  (list-ref song 1))
(define (song-artist song)
  (list-ref song 2))
(define (song-album song)
  (list-ref song 3))
(define (song-genre song)
  (list-ref song 4))
(define (song-track song)
  (list-ref song 5))

(define (song->sexp song)
  (let ((title (song-title song))
	(time (song-time song))
	(artist (song-artist song))
	(album (song-album song))
	(genre (song-genre song))
	(track (song-track song)))
    (list 'song title (time->sexp time) artist album genre track)))
