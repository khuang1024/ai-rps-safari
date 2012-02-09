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

;; deprecated: This function failed due to some error.
(defun compare-keys(t1 t2)
  (if (and (hash-table-p t1) (hash-table-p t2))
    (let (result nil)
      (maphash #'(lambda (k v) (cons (gethash k t2) results)) t1)
      (if (not (position nil results))
        t
        nil))
    (print "Compare failed: at least one of the inputs is not a hash table.")))

(defun show-avg-returns (&optional agent-list)
  ;; This function basically shows the values of *avg-returns*.
  (print (gethash 'r *avg-returns*))
  (print (gethash 'p *avg-returns*))
  (print (gethash 's *avg-returns*))
  (if agent-list
    (dolist (agnt agent-list)
      (print (gethash agnt *avg-returns*)))))


;; test

;; initialize the *avg-returns*
(setf *avg-returns* (make-hash-table :size 10))
(init-avg-returns) 
(init-avg-returns '(a1 a2 a3 a4))
(gethash 'r *avg-returns*)
(gethash 'p *avg-returns*)
(gethash 's *avg-returns*)
(gethash 'a1 *avg-returns*)
(gethash 'a2 *avg-returns*)
(gethash 'a3 *avg-returns*)
(gethash 'a4 *avg-returns*)

(show-avg-returns '(a1 a2))
(show-avg-returns '(a1 a2 a3 a4))

(setf t1 (make-hash-table :size 10))
(setf (gethash 'r t1) 1)
(setf (gethash 'p t1) 2)
(setf (gethash 's t1) 3)
(setf (gethash 'a1 t1) 4)
(setf (gethash 'a2 t1) 5)
(setf (gethash 'a3 t1) 6)
(setf (gethash 'a4 t1) 7)

(setf t2 (make-hash-table :size 10))
(setf (gethash 'r t2) 2)
(setf (gethash 'p t2) 2)
(setf (gethash 's t2) 1)
(setf (gethash 'a1 t2) 1)
(setf (gethash 'a2 t2) 2)
(setf (gethash 'a3 t2) 4)
(setf (gethash 'a4 t2) 1)

(update-avg-returns t1 1)
(update-avg-returns t2 2)
