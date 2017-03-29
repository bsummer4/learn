(define (or-func . args)
  (if (null? args)
      #f
      (or (car args) (apply or-func (cdr args)))))

(define (list-contains? list1 item)
  (apply or-func (map (lambda (x) (equal? x item)) list1)))

(define (whitespace? char1)
  (list-contains? '(#\newline #\space) char1))

(define (read-word port1 whitespace?)
  (define (reader)
    (let ((c (read-char port1)))
      (if (eof-object? c)
	  '()
	  (if (whitespace? c)
	      '()
	      (cons c (reader))))))

  (let ((c (peek-char port1)))
    (if (eof-object? c)
	c
	(let ((w (reader)))
	  (if (> (length w) 0)
	      (list->string w)
	      (read-word port1 whitespace?))))))


(define (read-all-words port1 whitespace?)
  (let ((w (read-word port1 whitespace?)))
    (if (eof-object? w)
	'()
	(cons w (read-all-words port1 whitespace?)))))


