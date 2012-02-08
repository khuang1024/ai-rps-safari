(defun cnt-if (predicate seq &key key)
  (if (null seq)
    0
    (if key
      (if (funcall predicate (funcall key (first seq)))
        (1+ (cnt-if predicate (rest seq) :key key))
        (cnt-if predicate (rest seq) :key key))
      (if (funcall predicate (first seq))
        (1+ (cnt-if predicate (rest seq)))
        (cnt-if predicate (rest seq))))))

(Cnt-If #'oddp '(1 2 3 4 5 6 7))
(Cnt-If #'oddp '((1 A) (2 B) (3 C) (4 D) (5 E) (6 F) (7 G)) :key #'first)
(Cnt-If #'oddp '((3 M) (1 A) (2 B) (3 C) (4 D) (5 E) (6 F) (7 G)) :key #'first)
(Cnt-If #'oddp '((2 B) (3 C) (4 D) (5 E) (6 F) (7 G)) :key #'first)
