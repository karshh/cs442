(define counter 0)

(define countme (lambda () (set! counter (+ 1 counter)) counter))

