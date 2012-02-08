(defun rmv2 (a seq &key key test)
  (if seq
    (if key
      (if test
        (if (funcall test (funcall key (first seq)) a) ; key, test
          (cons (first seq) (rmv2 a (rest seq) :key key :test test)) ; keep this item
          (rmv2 a (rest seq) :key key :test test)) ; remove this item
        (if (equal (funcall key (first seq)) a) ; key, no test
          (rmv2 a (rest seq) :key key) ; remove this item
          (cons (first seq) (rmv2 a (rest seq) :key key)))) ; keep this item
      (if test
        (if (funcall test (first seq) a) ; no key, test
          (cons (first seq) (rmv2 a (rest seq) :test test)) ; keep this item
          (rmv2 a (rest seq) :test test)) ; remove this item
        (if (equal (first seq) a)
          (rmv2 a (rest seq))
          (cons (first seq) (rmv2 a (rest seq))))))))

(Rmv2 'A '(B A N A N A))
(Rmv2 'A '((A B) (B A) (C A D) (D C A)) :key #'second)
(Rmv2 5 '(2 4 6 8) :test #'<)
(Rmv2 5 '((1 2) (1 4) (1 6) (1 8)) :key #'second :test #'<)

