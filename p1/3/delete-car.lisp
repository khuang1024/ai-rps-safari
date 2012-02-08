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
l

