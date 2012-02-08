(defun square (x)
  (* x x))

(defun 2+ (x)
  (+ x 2))

(defun sumx (predicate a b)
  (+ (funcall predicate a) (funcall predicate b)))

(defun square-pi-sum (predicate a b incre)
  (if (> a b)
    0
    (if (<= (funcall incre a) b)
      (+ (funcall predicate a) (square-pi-sum predicate (funcall incre a) b incre))
      (funcall predicate a))))

(Sumx #'Square 1 3)
(square-pi-sum #'Square 1 5 #'2+)
