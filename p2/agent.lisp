;;;; CMPS 240: Artificial Intelligence, P2 - agent 
;;; Kerui Huang
;;;
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
    (list '1 (nth (random 3) '(r p s))) ; for the first bet, just do a random bet
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
  ;; This function provides how much we bet for this round.
  (if (<= myscore 0)
    (if (= myscore (abs myscore))
      1
      -1)
    (progn 
      (setf x (ceiling (* 10 (atan (* 0.05 (- myscore 200))))))
      (if (> x 0)
        (if (> x myscore)
          (setf x myscore))
        (if (< x (- 0 myscore))
          (setf x (- 0 myscore))))
      (if (= x 0)
        (setf x 1))
;      (print "---------------------")
;      (print myscore)
;      (print x)
      x
      )))

(defun bet-choice (rounds)
  ;; This function chooses which one, rock, paper or scissor, to bet.
  (let ((r (- (third rounds) (second rounds)))
        (p (- (first rounds) (third rounds)))
        (s (- (second rounds) (first rounds))))
    (if (equal r (max r p s))
      (setf choice 'r)
      (if (equal p (max r p s))
        (setf choice 'p)
        (setf choice 's)))
;    (print choice)
    choice
    ))
