(defun cnt-if-not (predicate seq &key key)
  (if (null seq)
    0
    (if key
      (if (funcall predicate (funcall key (first seq)))
        (cnt-if-not predicate (rest seq) :key key)
        (1+ (cnt-if-not predicate (rest seq) :key key)))
      (if (funcall predicate (first seq))
        (cnt-if-not predicate (rest seq))
        (1+ (cnt-if-not predicate (rest seq)))))))

(Cnt-If-Not #'numberp '(A B C D 1 2))
(Cnt-If-Not #'numberp '((A 5) (B 4) (C 3) (D 2) (1 A) (2 B)) :key #'first)
(Cnt-If-Not #'numberp '((4 M) (A 5) (B 4) (C 3) (D 2) (1 A) (2 B)) :key #'first)
(Cnt-If-Not #'numberp '((M 4) (4 M) (A 5) (B 4) (C 3) (D 2) (1 A) (2 B)) :key #'first)
