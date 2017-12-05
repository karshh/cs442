(define kill3 (lambda (li)

    (set-cdr!  li (cons (car (cdr li))

        (cdddr li)))))

