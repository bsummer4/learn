(define (set . elements)
  "An unordered group of unique elements.  Non-unique elements are
   ignored.  "
  
  (if (null? elements)
      '()
      (let ((rest (apply set (list-tail elements 1)))
	    (first (list-ref elements 0)))
	
	;; warning: hackish!
	(let ((less? (if (string? first)
			 string<
			 >)))
	  (sort! 
	   (if (member first rest)
	       rest
	       (cons first rest))
	   less?)))))

(define (set-append set1 set2)
  (apply set (append set1 set2)))

(define (set-insert set1 element)
  (set-append (list element) set1))

