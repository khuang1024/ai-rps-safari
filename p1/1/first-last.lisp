(defun First-Last(lst)
  (if (rest lst)
    (list (first lst) (elt (First-Last (rest lst)) 1))
    (list (first lst) (first lst))))

 (First-Last '(a b c d e f))
 (First-Last '(a b c))

