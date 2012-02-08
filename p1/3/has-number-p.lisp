(defun has-number-p (p)
  (if p
    (if (listp p)
      (or (has-number-p (first p)) (has-number-p (rest p)))
      (if (numberp p)
        t))))

(has-number-p '(a (b (c d) (()))))
(has-number-p '(1 (2 (c))))
(has-number-p '(1 2 3 4))

(has-number-p 1)
(has-number-p 'a)
(has-number-p '(a (b (c d) ((3)))))
