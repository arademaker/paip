;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig
;;;; student.lisp: Chapter 7's STUDENT program to solve algebra word problems.

(in-package :student)

(defstruct (rule (:type list))
  pattern response)

(defstruct (exp (:type list)
		(:constructor mkexp (lhs op rhs)))
  op lhs rhs)


(defun exp-p (x) (consp x))
(defun exp-args (x) (rest x))

(pat-match-abbrev '?x* '(?* ?x))
(pat-match-abbrev '?y* '(?* ?y))

(defparameter *student-rules* (mapcar #'expand-pat-match-abbrev
				      '(((?x* |.|)                  ?x)
					((?x* |.| ?y*)          (?x ?y))
					((if ?x* |,| then ?y*)  (?x ?y))
					((if ?x* then ?y*)      (?x ?y))
					((if ?x* |,| ?y*)       (?x ?y))
					((?x* |,| and ?y*)      (?x ?y))
					((find ?x* and ?y*)     ((= to-find-1 ?x)
								 (= to-find-2 ?y)))
					((find ?x*)             (= to-find ?x))
					((?x* equals ?y*)       (= ?x ?y))
					((?x* same as ?y*)      (= ?x ?y))
					((?x* = ?y*)            (= ?x ?y))
					((?x* is equal to ?y*)  (= ?x ?y))
					((?x* is ?y*)           (= ?x ?y))
					((?x* - ?y*)            (- ?x ?y))
					((?x* minus ?y*)        (- ?x ?y))
					((difference between ?x* and ?y*)  (- ?y ?x))
					((difference ?x* and ?y*)          (- ?y ?x))
					((?x* + ?y*)            (+ ?x ?y))
					((?x* plus ?y*)         (+ ?x ?y))
					((sum ?x* and ?y*)      (+ ?x ?y))
					((product ?x* and ?y*)  (* ?x ?y))
					((?x* * ?y*)            (* ?x ?y))
					((?x* times ?y*)        (* ?x ?y))
					((?x* / ?y*)            (/ ?x ?y))
					((?x* per ?y*)          (/ ?x ?y))
					((?x* divided by ?y*)   (/ ?x ?y))
					((half ?x*)             (/ ?x 2))
					((one half ?x*)         (/ ?x 2))
					((twice ?x*)            (* 2 ?x))
					((square ?x*)           (* ?x ?x))
					((?x* % less than ?y*)  (* ?y (/ (- 100 ?x) 100)))
					((?x* % more than ?y*)  (* ?y (/ (+ 100 ?x) 100)))
					((?x* % ?y*)            (* (/ ?x 100) ?y)))))

(defun student (words)
  "Solve certain Algebra Word Problems."
  (solve-equations
   (create-list-of-equations
    (translate-to-expression (remove-if #'noise-word-p words)))))

(defun translate-to-expression (words)
  "Translate an English phrase into an equation or expression."
  (or (rule-based-translator
       words *student-rules*
       :rule-if #'rule-pattern :rule-then #'rule-response
       :action #'(lambda (bindings response)
		   (sublis (mapcar #'translate-pair bindings)
			   response)))
      (make-variable words)))

(defun translate-pair (pair)
  "Translate the value part of the pair into an equation or expression."
  (cons (binding-var pair)
	(translate-to-expression (binding-val pair))))

(defun create-list-of-equations (exp)
  "Separate out equations embedded in nested parens."
  (cond ((null exp) nil)
	((atom (first exp)) (list exp))
	(t (append (create-list-of-equations (first exp))
		   (create-list-of-equations (rest exp))))))

(defun noise-word-p (word)
  "Is this a low-content word which can be safely ignored?"
  (member word '(a an the this number of $)))

(defun make-variable (words)
  "Create a variable name based on the given list of words"
  (first words))

(defun solve-equations (equations)
  "Print the equations and their solution"
  (print-equations "The equations to be solved are:" equations)
  (print-equations "The solution is:" (solve equations nil)))

(defun solve (equations known)
  "Solve a system of equations by constraint propagation."
  ;; Try to solve for one equation, and substitute its value into
  ;; the others. If that doesn't work, return what is known.
  (or (some #'(lambda (equation)
		(let ((x (one-unknown equation)))
		  (when x
		    (let ((answer (solve-arithmetic
				   (isolate equation x))))
		      (solve (subst (exp-rhs answer) (exp-lhs answer)
				    (remove equation equations))
			     (cons answer known))))))
	    equations)
      known))

(defun isolate (e x)
  "Isolate the lone x in e on the left hand side of e."
  ;; This assumes there is exactly one x in e,
  ;; and that e is an equation.
  (cond ((eq (exp-lhs e) x)
	 ;; Case I: X = A -> X = n
	 e)
	((in-exp x (exp-rhs e))
	 ;; Case II: A = f(X) -> f(X) = A
	 (isolate (mkexp (exp-rhs e) '= (exp-lhs e)) x))
	((in-exp x (exp-lhs (exp-lhs e)))
	 ;; Case III: f(X)*A = B -> f(X) = B/A
	 (isolate (mkexp (exp-lhs (exp-lhs e)) '=
			 (mkexp (exp-rhs e)
				(inverse-op (exp-op (exp-lhs e)))
				(exp-rhs (exp-lhs e)))) x))
	((commutative-p (exp-op (exp-lhs e)))
	 ;; Case IV: A*f(X) = B -> f(X) = B/A
	 (isolate (mkexp (exp-rhs (exp-lhs e)) '=
			 (mkexp (exp-rhs e)
				(inverse-op (exp-op (exp-lhs e)))
				(exp-lhs (exp-lhs e)))) x))
	(t ;; Case V: A/f(X) = B -> f(X) = A/B
	 (isolate (mkexp (exp-rhs (exp-lhs e)) '=
			 (mkexp (exp-lhs (exp-lhs e))
				(exp-op (exp-lhs e))
				(exp-rhs e))) x))))

(defun print-equations (header equations)
  "Print a list of equations."
  (format t "~%~a~{~%  ~{ ~a~}~}~%" header
	  (mapcar #'prefix->infix equations)))

(defparameter operators-and-inverses
  '((+ -) (- +) (* /) (/ *) (= =)))

(defun inverse-op (op)
  (second (assoc op operators-and-inverses)))

(defun unknown-p (exp)
  (symbolp exp))

(defun in-exp (x exp)
  "True if x appears anywhere in exp"
  (or (eq x exp)
      (and (listp exp)
	   (or (in-exp x (exp-lhs exp)) (in-exp x (exp-rhs exp))))))

(defun no-unknown (exp)
  "Returns true if there are no unknowns in exp."
  (cond ((unknown-p exp) nil)
	((atom exp) t)
	((no-unknown (exp-lhs exp)) (no-unknown (exp-rhs exp)))
	(t nil)))

(defun one-unknown (exp)
  (let ((answer (find-one-unknown exp nil)))
    (if (eql answer 2)
        nil
        answer)))

(defun find-one-unknown (exp unknown)
  (cond ((eql unknown 2) 2)
        ((exp-p exp)
         (find-one-unknown
           (exp-rhs exp)
           (find-one-unknown (exp-lhs exp) unknown)))
        ((unknown-p exp)
         (if unknown
             2
             exp))
         (t unknown)))

(defun commutative-p (op)
  "Is operator commutative?"
  (member op '(+ * =)))

(defun solve-arithmetic (equation)
  "Do the arithmetic for the right hand side."
  ;; This assumes that the right hand side is in the right form.
  (mkexp (exp-lhs equation) '= (eval (exp-rhs equation))))

(defun binary-exp-p (x)
  (and (exp-p x) (= (length (exp-args x)) 2)))

(defun prefix->infix (exp)
  "Translate prefix to infix expressions."
  (if (atom exp) exp
      (mapcar #'prefix->infix
	      (if (binary-exp-p exp)
		  (list (exp-lhs exp) (exp-op exp) (exp-rhs exp))
		  exp))))

(defun solve-system (e u)
  ;;  inputs na forma ((= (+ (* a x) (* b y)) m) (= (+ (* c x) (* d y)) n))
  (multiple-value-bind (res binding)
      (pat-match `((= (+ (* ?A ,(first u)) (* ?B ,(second u))) ?M)
		   (= (+ (* ?C ,(first u)) (* ?D ,(second u))) ?N))
		 e)
  (if res
      (let ((a (cdr (assoc '?A binding)))
	    (b (cdr (assoc '?B binding)))
	    (c (cdr (assoc '?C binding)))
	    (d (cdr (assoc '?D binding)))
	    (m (cdr (assoc '?M binding)))
	    (n (cdr (assoc '?N binding))))
	(let ((y-f (eval (/ (- (* n a) (* m c)) (- (* d a) (* b c))))))
	(solve (subst y-f 'y (list (car e))) `((y . ,y-f))))))))

