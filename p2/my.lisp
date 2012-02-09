;;; 1. my legalp is added
;;; 2. random-agent is added
;;; 3. monitor is modified, eliminating undefined agents
;;; 4. *avg-returns* are added
;;; 5. manipulation functions for *avg-returns* are added
;;; 6. added flatten-results function for compressing results
;;; 7. added *avg-return*. see comments in tournament and monitor
;;;
;;; remarks:
;;;     run the monitor just by loading this source file in sbcl
;;;
;;;; CMPS 140 Tournament Monitor
;;;; Each agent starts with 200 points. There are 1000 rounds per tournament.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;   This is my legalp function.  ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun legalp (&optional parameters)
  ;; This function validate the play/input given by the player.
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
(setf *avg-returns* (make-hash-table :size 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;    My other functions, most of them manipulate *avg-returns*    ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun init-avg-returns (&optional agent-list)
  ;; This function is used for initializing *avg-returns*.
  (setf (gethash 'r *avg-returns*) 0)
  (setf (gethash 'p *avg-returns*) 0)
  (setf (gethash 's *avg-returns*) 0)
  (if agent-list
    (if (listp agent-list)
      (dolist (agnt agent-list)
        (setf (gethash agnt *avg-returns*) 0))
      (print "The input is not a list. Only initilize (r p s) in *avg-returns*."))
    (print "The input is null. Only initilize (r p s) in *avg-returns*.")))

(defun update-avg-returns (rps-agent-list so-far-numrounds)
  ;; This function updates *avg-returns* by using the hash table rps-agent-list which stores the increment value for
  ;; each key/value pair in *avg-returns*. Note that number of rounds so far should be indicated for average calculation.
  (if (and (hash-table-p rps-agent-list) (numberp so-far-numrounds))
    (if (= (hash-table-count *avg-returns*) (hash-table-count rps-agent-list))
      (maphash #'(lambda (k v)
                   (setf (gethash k *avg-returns*) (/ (+ (* v (1- so-far-numrounds)) (gethash k rps-agent-list)) so-far-numrounds)))
               *avg-returns*)
      (print "Update failed: the size of *avg-returns* and update table are not identical."))
    (print "Update failed: the first argument is not a hash table or the second argument is not a number.")))

(defun show-avg-returns (&optional agent-list)
  ;; This function basically shows the values of *avg-returns*.
  (print (gethash 'r *avg-returns*))
  (print (gethash 'p *avg-returns*))
  (print (gethash 's *avg-returns*))
  (if agent-list
    (dolist (agnt agent-list)
      (print (gethash agnt *avg-returns*)))))


(defun flatten-results(results)
  ;; This function add up the count of r,p,s
  (if results
    (progn
      (setf rps-table (make-hash-table :size 100))
      (setf (gethash 'r rps-table) 0)
      (setf (gethash 'p rps-table) 0)
      (setf (gethash 's rps-table) 0)
      (dotimes (th (length results))
        (setf (gethash 'r rps-table) (+ (gethash 'r rps-table) (first (nth th results))))
        (setf (gethash 'p rps-table) (+ (gethash 'p rps-table) (second (nth th results))))
        (setf (gethash 's rps-table) (+ (gethash 's rps-table) (third (nth th results)))))
      rps-table)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sign (input)
  (if (= input 0)
      0
      (if (< input 0)
	  -1
	  1)))	      
;;;; (sign 2)

(defun tournament (agents numtimes numtournament)
  ;; This function is modified a little bit by adding a new argument -- numtournament.
  ;; This is for calculating *avg-returns*.
  (let ((scores (make-list (length agents) :initial-element 200))
	(net-scores nil) (results nil) (legal-status (make-list (length agents) :initial-element t)))
    (print "Running tournament")
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
        (push (list r p s) results)))
      (dotimes (x (length agents))
        (if (nth x legal-status)
          (push (list (nth x agents) (nth x scores)) net-scores)
          nil))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;(print net-scores)
      ;(print results)
      (setf rps-agent-update-table (flatten-results results)) ; put r, p, s into table
      (dolist (agent-score net-scores) ; put agents into table
        (setf (gethash (first agent-score) rps-agent-update-table) (second agent-score)))
      ;(print rps-agent-update-table)
      ;(init-avg-returns agents) ; this should actually be in monitor, here for test

      ; don't forget to add 1 to numtournament! the first value of numtournament is 0!
      (update-avg-returns rps-agent-update-table (1+ numtournament)) 
      (show-avg-returns agents)
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
      (let ((curRank 0) (results (tournament agent-list 10 x)))
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

;; Returns a list of dotted pairs, with the agent and the score (numagents - avg rank)
(monitor '(simple-agent simple-agent2 simple-agent3 random-agent ) 3)

;(monitor agentlist 1000)
