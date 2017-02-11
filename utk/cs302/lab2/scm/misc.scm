(define (replace-chars string replace)
  (list->string(map replace (string->list string))))

;;(replace-chars "foo_bar" (lambda (c) (if (char=? c #\_) #\space c)))

;; fixme: this is not tested or good:
(define (sort list less?)
  (if (null? list)
      '()
      (let ((pivot (list-ref list o)))
	(let ((less
	       (reduce (lambda (thing1)
			 (if (less? pivot thing2)))
		       list))
	      (greater
	       (reduce (lambda (thing1)
			 (not (less? pivot thing2)
			      (eq? pivot thing2)))
		       list)))
	  (append
	   (sort less less?)
	   pivot
	   (sort greater less?))))))