(define (read-line port)
  (define (reader)
    (let ((char (read-char port)))
      (if (or (eq? #\newline char)
	      (eof-object? char))
	  '()
	  (cons char (reader)))))

  ;fixme: This checks the first charecter for a newline; It could
  ;       probably be writtean more elegantly (it's pretty much a
  ;       duplicate of the above code.)
  (let ((char (read-char port)))
    (cond ((eq? #\newline char) "")
	  ((eof-object? char) char)
	  (else (list->string (cons char (reader)))))))

(define (read-lines port)
  (let ((line (read-line port)))
    (if (eof-object? line)
	'()
	(cons line (read-lines port)))))
