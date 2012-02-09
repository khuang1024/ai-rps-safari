(defun legalp (&optional parameters)
  ;; This function validate the play/input given by the player.
  (if parameters
    (if (listp parameters)
      (and
        (= (length parameters) 3) ; there should be only three parameters
        (atom (first parameters)) ; bet is atom rather than cons
        (atom (second parameters)) ; choice is atom rather than cons
        (atom (third parameters)) ; score is atom rather than cons
        (member (second parameters) '(r p s)) ; choice must be one of r p s
        (not (null (first parameters))) ; bet cannot be nil
        (not (null (second parameters))) ; choice cannot be nil
        (not (null (third parameters))) ; score cannot be nil
        (not (= (first parameters) 0)) ; bet cannot be 0
        (if (> (third parameters) 0)
          ; when score > 0
          (if (< (third parameters) (abs (first parameters)))
            nil
            t)
          ; when score <= 0
          (if (= 1 (abs (first parameters)))
            t
            nil))))))

;;; test cases
(legalp '(1 r 1)) ; t
(legalp '(5 r 6)) ; t
(legalp '(-5 r 6)) ; t
(legalp '(5 r 5)) ; t
(legalp '(1 r 0)) ; t 
(legalp '(-1 r 0)) ; t 
(legalp '(1 r 0)) ; t 
(legalp '(1 r -1)) ; t 
(legalp '(1 k 1)) ; nil
(legalp '(5 r 4)) ; nil
(legalp '(2 r 0)) ; nil
(legalp '(0 r 0)) ; nil
(legalp '(1 '(r p s) 5)) ; nil
(legalp '('(1 2) 'r 5)) ; nil
(legalp '(1 2)) ; nil 
(legalp) ; nil 
