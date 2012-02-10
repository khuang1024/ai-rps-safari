;;; 1. This agent takes the total numbers of rock, paper, scissor into
;;; account. Based on how scores are calculated and the whole history of
;;; rock, paper, scissor, it chooses the best one which is most possible to
;;; win.
;;;
;;; 2. On the other hand, this agent weigh the bet based on its previous
;;; performance, which means it looks at its current score and figure out
;;; how it performs so far. If the score is greater than its intial score,
;;; which means it performs well, it makes a positive bet based on how well
;;; it performs. How well it performs is formulated by a matchematical
;;; expression by using arctan function. The function is
;;; 10*arctan(0.05*(x-200)) where x is the agent's current score and 200 is
;;; the default starting points it used to have.


(defun agent(rounds scores myscore)
  ;; This function is the agent function which is supposed to be called
  ;; when it is competing with other agents in tournament.
  (if (or (null rounds) (null scores) (null myscore))
    (list '1 (nth (random 3) '(r p s))) ; if the input is initial state, just do a random bet
    (if (not (and (listp rounds) (listp scores) (numberp myscore)))
      (list '1 (nth (random 3) '(r p s))) ; if the input is invalid, just do a random bet
      (progn
        (setf frounds (flatten-rounds rounds))
        (list (bet-weight myscore) (bet-choice frounds))))))

(defun flatten-rounds (rounds)
  ;; This function sums up all rock, paper and scissor
  (if rounds
    (let ((r 0) (p 0) (s 0))
      (dolist (element rounds)
        (setf r (+ r (first element)))
        (setf p (+ p (second element)))
        (setf s (+ s (third element))))
      (list r p s))))

(defun bet-weight (myscore)
  (progn 
    (setf x (ceiling (* 10 (atan (* 0.05 (- myscore 200))))))
    (numberp x)
    x))

(defun bet-choice (rounds)
  ;; This function makes the bet decision.
  (let ((r (- (third rounds) (second rounds)))
        (p (- (first rounds) (third rounds)))
        (s (- (second rounds) (first rounds))))
    (if (equal r (max r p s))
      (setf choice 'r)
      (if (equal p (max r p s))
        (setf choice 'p)
        (setf choice 's)))
    choice))

;; test cases
(setf y1 '(1 2 3))
(setf y2 '(3 3 3))
(setf y3 '(4 2 1))
(setf y4 '(3 9 2))
(setf rounds (list y1 y2 y3 y4))
(setf scores '(210 200 190 200))
(setf bid (agent rounds scores 230))
;(funcall x-agent rounds scores 230)
;(numberp (first bid))
;(setf funs (list x-agent x-agent x-agent))
;(first (funcall (first funs) rounds scores 230)
