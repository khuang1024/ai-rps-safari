(in-package :khuang7-p3)

(let ((my-score (list 1)))
    (defun agent(rounds scores myscore)
      ;; This function is the agent function which is supposed to be called
      ;; when it is competing with other agents in tournament.
      (if (or (null rounds) (null scores) (null myscore))
        (list '1 (nth (random 3) '(r p s))) ; for the first bet, just do a random bet
        (if (not (and (listp rounds) (listp scores) (numberp myscore)))
          (list '1 (nth (random 3) '(r p s))) ; if the input is invalid, just do a random bet
          (progn
            (setf my-score (cons myscore my-score))
            ;(setf frounds (flatten-rounds rounds))
            (list (bet-weight2 my-score 0.5) (bet-choice2 rounds)))))))

(defun hist (my-score n m)
  ;; The default value of n is 10.
  ;; n is the weight of current play.
  ;; m is the decrement of the weight of the previous play.
  (if (> (length my-score) 2) ; when at least two records
    (if (> n 0)
      (progn
        (setf currscore (first my-score))
        (setf lastscore (first (rest my-score)))
        (+ (* n (- currscore lastscore)) (hist (rest my-score) (- n m) m)))
      0)
    0))

(defun bet-weight2 (my-score tune)
  ;; This function provides how much we bet for this round.
  ;; Tune is the weight of past delta, which must be between 0 and 1.
  (progn
    (setf current-score (first my-score))
    (setf absolute-delta (- current-score 1))
    (setf s (* 0.01 (+ (* (hist my-score 10 1) tune) (* (- 1 tune) current-score))))
    (setf w (ceiling (+ 14 (* 10 (atan (* 0.05 (- s 100)))))))
    (if (> w 0)
      (if (> w current-score)
        (setf w current-score))
      (if (< w (- 0 current-score))
        (setf w (- 0 current-score))))
    (if (= w 0)
      (setf w 1))
    w))

(defun bet-choice2 (rounds)
  ;; This function chooses which one, rock, paper or scissor, to bet.
  (if (and (= 0 (first *total-returns*))
           (= 0 (second *total-returns*))
           (= 0 (third *total-returns*)))
    (nth (random 3) '(r p s)) ; if it is the first time, random guess
    (progn
      (setf rock (first *total-returns*))
      (setf paper (second *total-returns*))
      (setf scissor (third *total-returns*))
      (if (= rock (min rock paper scissor))
        'r
        (if (= paper (min rock paper scissor))
          'p
          's)))))

