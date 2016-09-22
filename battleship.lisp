;; Battleship game
;; Tales Rands

(in-package :battleship)

(defun create-board (n)
  (defparameter *board* `(,(create-ships n))))

(setf (get 'translate-attack '#\A) '0)
(setf (get 'translate-attack '#\B) '10)
(setf (get 'translate-attack '#\C) '20)
(setf (get 'translate-attack '#\D) '30)
(setf (get 'translate-attack '#\E) '40)
(setf (get 'translate-attack '#\F) '50)
(setf (get 'translate-attack '#\G) '60)
(setf (get 'translate-attack '#\H) '70)
(setf (get 'translate-attack '#\I) '80)
(setf (get 'translate-attack '#\J) '90)

(defun create-ships (n &optional (ships nil))
  (if (= n 0)
      ships
      (let ((nship (new-ship)))
	(if (null (intersection nship (flatten ships)))
	    (create-ships (- n 1) (cons nship ships))
	    (create-ships n ships)))))

(defun new-ship ()
  (let ((n (random 100)) (dir (- 10 (* 9 (random 2)))))
    (if (= dir 10)
	(cond ((>= n 90)
	       (list (- n 20) (- n 10) n))
	      ((<= n 9)
	       (list n (+ n 10) (+ n 20)))
	      (t (list (- n dir) n (+ n dir))))
	(cond ((= (mod n 10) 0)
	       (list n (+ n 1) (+ n 2)))
	      ((= (mod n 10) 9)
	       (list (- n 2) (- n 1) n))
	      (t (list (- n dir) n (+ n dir)))))))

(defun update-board (attack)
  (setq *board* (cons (sink-test attack) (cons attack (cdr *board*)))))

(defun sink-test (attack)
  (let ((ships (remove-if #'(lambda (list) (subsetp list (cons attack (cdr *board*))))
			  (car *board*))))
    (if (not (equal ships (car *board*)))
	(print "You sank a ship!"))
    ships))
	

(defun in-board (attack)
  (and (numberp attack) (<= attack 99) (>= attack 0)))

(defun attack-result (attack)
  (cond ((member attack (cdr *board*))
	 '(You have shot here already))
	((member attack (flatten (first *board*)))
	 (update-board attack)
	 (if (subsetp (flatten (first *board*)) (flatten (cdr *board*)))
	     '(CONGRATULATIONS! YOU WON!)
	     '(You hit something!)))
	(t (cond ((in-board attack)
		  (update-board attack)
		  (list 'Water!))
		 (t '(Our board is a 10x10 square - Please choose your attack from A to J and from 0 to 9.))))))

(defun translate-attack (target)
  (let ((n (digit-char-p (elt (symbol-name target) 1))))
       (+ (if (numberp n)
	      n
	      '100)
	  (get 'translate-attack (elt (symbol-name target) 0) 200))))

(defun battleship ()
  (format t "~{~A ~}" '(Hello! We play in a 10x10 board. Please choose the number of ships you want on the board))
  (create-board (read))
  (format t "~{~A ~}" '(Please indicate your attack by a letter between A and J and a number between 0 and 9. Example C4))
    (loop
       (print 'Your-move>)
       (let ((answer (attack-result (translate-attack (read)))))
	 (format t "~{~A ~}" answer))))
