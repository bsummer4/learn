(define (underscore->space string)
  (replace-chars string (lambda (c) (if (char=? c #\_) #\space c))))

(define (filter list test)
  (if (null? list)
      '()
      (let ((first (list-ref list 0)))
	(let ((result (filter (list-tail list 1) test)))
	  (if (test first)
	      (cons first result)
	      result)))))

(define (replace-chars string replace)
  (list->string(map replace (string->list string))))

(define (pad string num char)
  "Makes sure that a string is at least 'num long.  Otherwise, it
   pads the fron with 'char s"
  (if (>= (string-length string) num)
      string
      (pad (list->string (cons char (string->list string)))
	   num
	   char)))


;; fixme: this is not tested or good:
; (define (sort list less?)
;;   (if (null? list)
;;       '()
;;       (let ((pivot (list-ref list o)))
;; 	(let ((less
;; 	       (reduce (lambda (thing1)
;; 			 (if (less? pivot thing2)))
;; 		       list))
;; 	      (greater
;; 	       (reduce (lambda (thing1)
;; 			 (not (less? pivot thing2)
;; 			      (eq? pivot thing2)))
;; 		       list)))
;; 	  (append
;; 	   (sort less less?)
;; 	   pivot
;; 	   (sort greater less?))))))


;; This is not my code!
(define (merge! a b less?)
    (define (loop r a b)
         (if (less? (car b) (car a))
             (begin
                 (set-cdr! r b)
                 (if (null? (cdr b))
                     (set-cdr! b a)
                     (loop b a (cdr b)) ))
             ;; (car a) <= (car b)
             (begin
                 (set-cdr! r a)
                 (if (null? (cdr a))
                     (set-cdr! a b)
                     (loop a (cdr a) b)) )) )
    (cond
         ((null? a) b)
         ((null? b) a)
         ((less? (car b) (car a))
             (if (null? (cdr b))
                 (set-cdr! b a)
                 (loop b a (cdr b)))
             b)
         (else ; (car a) <= (car b)
             (if (null? (cdr a))
                 (set-cdr! a b)
                 (loop a (cdr a) b))
             a)))


(define (sort! seq less?)
    (define (step n)
         (cond
             ((> n 2)
                 (let* ((j (quotient n 2))
                        (a (step j))
                        (k (- n j))
                        (b (step k)))
                     (merge! a b less?)))
             ((= n 2)
                 (let ((x (car seq))
                       (y (cadr seq))
                       (p seq))
                     (set! seq (cddr seq))
                     (if (less? y x) (begin
                         (set-car! p y)
                         (set-car! (cdr p) x)))
                     (set-cdr! (cdr p) '())
                     p))
             ((= n 1)
                 (let ((p seq))
                     (set! seq (cdr seq))
                     (set-cdr! p '())
                     p))
             (else
                 '()) ))
    (if (vector? seq)
         (let ((n (vector-length seq))
               (vector seq))                     ; save original vector
             (set! seq (vector->list seq))       ; convert to list
             (do ((p (step n) (cdr p))           ; sort list destructively
                  (i 0 (+ i 1)))                         ; and store elements back
                 ((null? p) vector)              ; in original vector
                 (vector-set! vector i (car p)) ))
         ;; otherwise, assume it is a list
         (step (length seq))))



