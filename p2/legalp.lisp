(defun legalp (bet choice score)
  ;; This function validate the play/input given by the player.
  (and
    (atom bet) ; bet is atom rather than cons
    (atom choice) ; choice is atom rather than cons
    (atom score) ; score is atom rather than cons
    (member choice '(r p s)) ; choice must be one of r p s
    (not (null bet)) ; bet cannot be nil
    (not (null choice)) ; choice cannot be nil
    (not (null score)) ; score cannot be nil
    (not (= bet 0)) ; bet cannot be 0
    (if (> score 0)
       ; when score > 0
       (if (< score (abs bet))
         nil
         t)
       ; when score <= 0
       (if (= 1 (abs bet))
         t
         nil))))

;;; test cases
#|
(legalp 1 'r 1) ; t
(legalp 5 'r 6) ; t
(legalp -5 'r 6) ; t
(legalp 5 'r 5) ; t
(legalp 1 'r 0) ; t 
(legalp -1 'r 0) ; t 
(legalp 1 'r 0) ; t 
(legalp 1 'r -1) ; t 
(legalp 1 'k 1) ; nil
(legalp 5 'r 4) ; nil
(legalp 2 'r 0) ; nill
(legalp 0 'r 0) ; nil
(legalp 1 '(r p s) 5) ; nil
(legalp '(1 2) 'r 5) ; nil
|#
