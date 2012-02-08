(defun Fibonacci(i)
  (if (= i 0)
    0
    (if (= i 1)
      1
      (+ (Fibonacci (- i 1)) (Fibonacci (- i 2))))))

(Fibonacci 7)
(Fibonacci 8)
