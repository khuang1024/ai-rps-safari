(defun sum-of-squares (lst)
  (if lst
    (+ (* (first lst) (first lst)) (sum-of-squares (rest lst)))
    0))


(Sum-of-Squares '(0 1 2 3 4))
