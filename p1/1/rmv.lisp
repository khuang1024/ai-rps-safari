(defun rmv (a lst)
  (if lst
    (if a
      (if (equal a (first lst))
        (rmv a (rest lst))
        (cons (first lst) (rmv a (rest lst)))))))

(Rmv 'a '(b a n a n a))
(Rmv 'a '(b n a n a))
(Rmv 'a '(n a n a))
