(define (ascending? s) 
    (if (<= (length s) 1) #t
    (let((x (car s)) (y (car (cdr s))))
        (if (<= x y) (ascending? (cdr s)) #f)
    ))
)

(define (my-filter pred s)
   (if (zero? (length s)) '() 
    (if (pred (car s)) (cons(car s) (my-filter pred (cdr s))) (my-filter pred (cdr s)))
   )
)

(define (interleave lst1 lst2) 
    (cond
        ((zero? (length lst1)) lst2)
        ((zero? (length lst2)) lst1)
        (else (cons(car lst1) (interleave lst2 (cdr lst1))))
    )
)

(define (no-repeats s)
  (if (null? s) s
    (cons (car s)
      (no-repeats (filter (lambda (x) (not (= (car s) x))) (cdr s)))))
)
