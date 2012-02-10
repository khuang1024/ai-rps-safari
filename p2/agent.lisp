(defun my-agent(rounds scores myscore)
  ;; This function is the agent function which is supposed to be called
  ;; when it is competing with other agents in tournament.
  (if (not (and (listp rounds) (listp scores) (numberp myscore)))
    (list '1 (nth (random 3) '(r p s))) ; if the input is invalid, just do a random bet
    (progn
      (setf frounds (flatten-rounds rounds))
      (setf mybet (bet frounds myscore))
      mybet)))

(defun flatten-rounds (rounds)
  ;; This function sums up all rock, paper and scissor
  (if rounds
    (let ((r 0) (p 0) (s 0))
      (dolist (element rounds)
        (setf r (+ r (first element)))
        (setf p (+ p (second element)))
        (setf s (+ s (third element))))
      (list r p s))))

(defun bet(rounds myscore)
  ;; This function makes the bet decision.
  (let ((r (- (third rounds) (second rounds)))
        (p (- (first rounds) (third rounds)))
        (s (- (second rounds) (first rounds))))
    (if (equal r (max r p s))
      (setf choice 'r)
      (if (equal p (max r p s))
        (setf choice 'p)
        (setf choice 's)))

    ;; how much is the bet?
    (setf bid (* 10 (atan (* 0.05 (- myscore 200)))))
    (list bid choice)))

(defun avg (scores)
  ;; This function calculates the average scores of all these agents.
  (/ (reduce #'+ scores) (length scores)))



;; test cases
(setf y1 '(1 2 3))
(setf y2 '(3 3 3))
(setf y3 '(4 2 1))
(setf y4 '(3 9 2))
(setf y (list y1 y2 y3 y4))

(setf rounds y)
(setf scores '(210 200 190 200))
(my-agent rounds scores 230)
