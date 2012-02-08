;;; Problem 2 from:
;;; http://www.apl.jhu.edu/~hall/AI-Programming/HW1-Review.html

(defun Random-Range(a b)
  (if (= a b)
    a
    (if (< a b)
      (+ a (random (1+ (- b a))))
      (+ b (random (1+ (- a b)))))))

(Random-Range 1 1)
(Random-Range 1 2)
(Random-Range 5 10)
(Random-Range 13 8)


;;; Problem 3 from:
;;; http://www.apl.jhu.edu/~hall/AI-Programming/HW1-Review.html
(defun sum-of-squares (lst)
  (if lst
    (+ (* (first lst) (first lst)) (sum-of-squares (rest lst)))
    0))

(Sum-of-Squares '(0 1 2 3 4))


;;; Problem 4 from:
;;; http://www.apl.jhu.edu/~hall/AI-Programming/HW1-Review.html
(defun First-Last(lst)
  (if (rest lst)
    (list (first lst) (elt (First-Last (rest lst)) 1))
    (list (first lst) (first lst))))

(First-Last '(a b c d e f))
(First-Last '(a b c))


;;; Problem 5a from:
;;; http://www.apl.jhu.edu/~hall/AI-Programming/HW1-Review.html
(defun Fibonacci(i)
  (if (= i 0)
    0
    (if (= i 1)
      1
      (+ (Fibonacci (- i 1)) (Fibonacci (- i 2))))))

(Fibonacci 7)
(Fibonacci 8)


;;; Problem 7 from:
;;; http://www.apl.jhu.edu/~hall/AI-Programming/HW1-Review.html
(defun rmv (a lst)
  (if lst
    (if a
      (if (equal a (first lst))
        (rmv a (rest lst))
        (cons (first lst) (rmv a (rest lst)))))))

(Rmv 'a '(b a n a n a))
(Rmv 'a '(b n a n a))
(Rmv 'a '(n a n a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Problem 1 from:
;;; http://www.apl.jhu.edu/~hall/AI-Programming/HW2-Higher-Order.html
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

;;; Problem 2 from:
;;; http://www.apl.jhu.edu/~hall/AI-Programming/HW2-Higher-Order.html
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


;;; Problem 3 from:
;;; http://www.apl.jhu.edu/~hall/AI-Programming/HW2-Higher-Order.html
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

;;; Problem 4 from:
;;; http://www.apl.jhu.edu/~hall/AI-Programming/HW2-Higher-Order.html
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Problem 1 from:
;;; http://www.cs.northwestern.edu/academics/courses/325/exercises/lisp-exs.html
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


;;; Problem 3 from:
;;; http://www.cs.northwestern.edu/academics/courses/325/exercises/lisp-exs.html
(defun make-balance (balance)
  (if balance
    #'(lambda (&optional incre)
         (if incre
           (setf balance (+ incre balance))
           (print balance)))))

(setq bal (make-balance 100))
(funcall bal 10)
(funcall bal -50)
(funcall bal)

;;; Problem 4 from:
;;; http://www.cs.northwestern.edu/academics/courses/325/exercises/lisp-exs.html
(defun delete-car (l)
  (if l
    (progn
      (loop for i from 0 to (1- (length l))
            do (setf (nth i l) (nth (1+ i) l)))
      (delete nil l :from-end t :count 1))))


(setq l (list 'a 'b 'c))
(delete-car l)
l
(delete-car l)


