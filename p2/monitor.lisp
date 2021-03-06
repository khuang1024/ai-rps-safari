;;; CMPS 240: Artificial Intelligence, P2 - monitor
;;; Kerui Huang

(defun simple-agent (rounds scores myscore)
  '(1 R))
(defun simple-agent2 (rounds scores myscore)
  '(1 P))
(defun simple-agent3 (rounds scores myscore)
  '(1 S))
(defun illegal-agent (rounds scores myscore)
  '(0 P))
(defun random-agent (rounds scores myscore)
  (list '1 (nth (random 3)
		'(R P S))))
(defun verbose-agent (r s m)
  (let ((shoot (nth (random 3)
		'(R P S))) (amount (+ 1 (random 4))))
  (list amount shoot)))


(defun replace-nth (list n elem)
  (cond 
    ((null list) ())
    ((equal n 0) (cons elem (cdr list)))
    (t (cons (car list) (replace-nth (cdr list) (- n 1) elem)))))


;;; Monitor takes a list of agents, returns an ordered list of winners.
(defun sort-agents (agent-scores)
  (sort agent-scores #'(lambda (x y) (< (second x) (second y)))))


(defun compress-agents (agents)
  (setq agents (sort-agents agents))
  (let ((results (list (first (first agents)))) (lastscore (second (first agents))))
    (dolist (agnt (cdr agents))
 	(if (listp (car results))
	    (if (= (second agnt) lastscore)
		(setq results (cons (cons (first agnt) (first results)) (rest results)))
		(progn
		  (setq lastscore (second agnt))
		  (push (first agnt) results)))
	    (if (= (second agnt) lastscore)
		(setq results (cons (cons (first agnt) (cons (first results) nil)) (rest results)))
		(progn
		  (setq lastscore (second agnt))
		  (push (first agnt) results)))))
    results))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;   This is my legalp function.  ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun legalp (&optional parameters)
  ;; This function validates the play/input given by the agent.
  (if parameters ; when the input is null, return false
    (if (listp parameters) ; when the input is not a list, return false
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

;; create the global list *avg-returns*
(defparameter *avg-returns* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;    My other functions, most of them manipulate *avg-returns*    ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init-avg-returns (agents)
  ;; This function initializes/resets *avg-returns*.
  (setf *avg-returns* (make-list (+ 3 (length agents)) :initial-element 0)))

(defun update-avg-returns (scores results)
  ;; This function updates *avg-returns* each round based on
  ;; scores and results.
  (let ((rounds (length results)) (r 0) (p 0) (s 0))
    (dotimes (x rounds)
      (setf r (+ r (first (nth x results))))
      (setf p (+ p (second (nth x results))))
      (setf s (+ s (third (nth x results)))))
    (setf (first *avg-returns*) (/ r rounds))
    (setf (second *avg-returns*) (/ p rounds))
    (setf (third *avg-returns*) (/ s rounds))
    (dotimes (y (length scores))
      (setf (nth (+ 3 y) *avg-returns*) (/ (nth y scores) rounds)))
    *avg-returns*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sign (input)
  (if (= input 0)
      0
      (if (< input 0)
	  -1
	  1)))	      
;;;; (sign 2)


(defun tournament (agents numtimes)
  (let ((scores (make-list (length agents) :initial-element 1))
	(net-scores nil) (results nil) (legal-status (make-list (length agents) :initial-element t)))
    (print "Running tournament")
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; reset *avg-returns*
    (init-avg-returns agents)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (do ((iter 0 (setq iter (+ iter 1))))
	  ((= iter numtimes))
      (let ((num 0) (r 0) (p 0) (s 0) (current-play nil))
	    ;; Run each agent
	    (dolist (curagent agents)
	      (let* ((plyval (funcall curagent results scores (nth num scores))) (bid (first plyval)))
		(if (legalp (list (first plyval) (second plyval) (nth num scores)))
		    (progn 
		      (case (second plyval)
			(r (setq r (+ r bid)))
			(p (setq p (+ p bid)))
			(s (setq s (+ s bid)))
			(t 0))
		      (setf num (+ 1 num))
		      (push plyval current-play))
		    (progn
		      (setq legal-status (replace-nth legal-status num nil))
		      (push plyval current-play)
		      (setq num (+ 1 num))))))
	    (setq current-play (reverse current-play))
	    (dotimes (frmagent (length agents))
	      (let* ((curagent frmagent) (play (nth curagent current-play)) (shoot (second play)) (bid (first play)))
            (if (nth curagent legal-status)
              (setq scores (replace-nth scores curagent (+ (nth curagent scores)
                                                           (* bid
                                                              (sign (case shoot
                                                                      (r (- s p))
                                                                      (p (- r s))
                                                                      (s (- p r))
                                                                      (t 0)))))))
              (setq scores (replace-nth scores curagent 0)))))
        (push (list r p s) results)
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; (print "====================================")
        ;; (print results)
        ;; (print scores)
        ;; (print *avg-returns*)
        ;; (print "-------------")
        (update-avg-returns scores results)
        ;; (print *avg-returns*)
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ))
      (dotimes (x (length agents))
        (if (nth x legal-status)
          (push (list (nth x agents) (nth x scores)) net-scores)
          nil))
      (print (reverse (compress-agents net-scores)))))
    
;(tournament '(simple-agent simple-agent2 simple-agent3 random-agent illegal-agent) 100)
;(tournament '(simple-agent simple-agent2 simple-agent3 random-agent) 10 1)


;;;; Takes a list of agents, inserts them in a hash-table and passes theo
;;;; hash-table to the tournament numtimes times
(defun monitor (agent-list numtimes)
  (let (results (agents (make-hash-table :size 100)))
    (init-avg-returns agent-list)
    (dolist (agnt agent-list)
      (setf (gethash agnt agents) 0))
    (dotimes (x numtimes)
      (let ((curRank 0) (results (tournament agent-list 1000)))
        (dolist (agent-result results)
          (if (listp agent-result)
            (progn
              (dolist (curagent agent-result)
                (setf (gethash curagent agents) (+ (gethash curagent agents) curRank)))
              (setq curRank (+ curRank (length agent-result))))
            (progn
              (setf (gethash agent-result agents) (+ (gethash agent-result agents) curRank))
              (setq curRank (+ curRank 1)))))))
    (maphash #'(lambda (k v) (push (cons k (- (length agent-list) (ceiling (/ v numtimes)))) results)) agents)
    results))

; Returns a list of dotted pairs, with the agent and the score (numagents - avg rank)
; (monitor '(random-agent random-agent random-agent agent) 1)

;(monitor agentlist 1000)
