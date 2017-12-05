(define s-car car)
(define s-cdr (lambda (s) (force (cdr s))))

(define lazymap
  (lambda (f x)
    (cons (f (s-car x))
          (delay (lazymap f (s-cdr x))))))
