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
