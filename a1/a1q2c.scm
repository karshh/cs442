+ 

    (letrec 

      ((rangelist (lambda (x y) (if (> x y) '() (cons x (rangelist (+ x 1) y)))))) 

      (rangelist 1 (length lis)))


