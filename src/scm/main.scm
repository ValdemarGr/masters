(define (f x) 
  (+ x x))

(display (string-append (number->string (f 2)) "\n"))

(define (fib n) 
  (if (> n 1)
      (+ (fib (- n 1)) (fib (- n 2)))
      1))

(display (string-append (number->string (fib 10)) "\n"))

(define hello 
  (let ((a "hello\n")) (string-append a "world\n")))

(display hello)

(define main
  ((lambda (add) ((add 1) 2)) (lambda (x) (lambda (y) (+ x y)))))

(display main)
