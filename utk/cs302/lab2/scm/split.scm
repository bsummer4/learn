(define (whitespace? c)
  (or
   (eq? c #\newline)
   (eq? c #\space)))

(define (split-string s test)
  ;;"Split a string into a list of words"
  
  (define (get-chars l test)
    ;;"Get one segment of a list of charecters that passes *test*.  "
    (if (null? l)
	  '()
	  (let ((char (car l)))
	    (if (not (test char))
		'()
		(cons char (get-chars (cdr l) test))))))
  
  (define (all-words l)
    (let ((space (get-chars l test)))
      (let ((word (get-chars (list-tail l (length space))
			     (lambda (c) (not (test c))))))
	(let ((len (+ (length word) (length space))))
	  (if (null? word)
	      '()
	      (cons
	       (list->string word)
	       (all-words (list-tail l len))))))))
    (all-words (string->list s)))