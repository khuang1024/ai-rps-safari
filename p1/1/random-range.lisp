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
