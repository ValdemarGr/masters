(format t "Hello, world!")

(defun add2 (x) 
  (write-to-string (+ 2 x)))

(defun applyLambda2 (fn)
  (funcall fn 2))

(defun pl () 
  (applyLambda2 #'(lambda (x) (* x 2))))

(format t (write-to-string (pl)))

;(defun foo (fn)
  ;(mapcar fn '(1 2 3)))

;(foldl (lambda (a b) (+ a b)) 0 (foo #'sqrt))

;(reduce #'+ 0 '(1 2 3 4))

;(pl)

;(format t (write-to-string (main (ambda (a b) (+ a b)))))
