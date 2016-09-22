
(in-package :symbolic-math)

(defun infix->prefix (infix-exp)
  "Convert fully parenthesized infix-exp to a prefix expression"
  ;; Don't use this version for non-fully parenthesized exps!
  (prefix->infix infix-exp))

(defun infix->prefix (exp)
  "Translate an infix expression into prefix notation."
  ;; Note we cannot do implicit multiplication in this system
  (cond ((atom exp) exp)
	((= (length exp) 1) (infix->prefix (first exp)))
	((rule-based-translator exp *infix->prefix-rules*
		:rule-if #'rule-pattern :rule-then #'rule-response
		:action
		#'(lambda (bindings response)
		    (sublis (mapcar
			      #'(lambda (pair)
				  (cons (first pair)
					(infix->prefix (rest pair))))
			      bindings)
			     response))))
	((symbolp (first exp))
	 (list (first exp) (infix->prefix (rest exp))))
	(t (error "Illegal exp"))))

(defun variable-p (exp)
  (member exp '((x y z m n o p q r s u v w) T)))

(pat-match-abbrev 'x+ '(?+ x))
(pat-match-abbrev 'y+ '(?+ y))

(defun rule-pattern (rule) (first rule))
(defun rule-response (rule) (second rule))

(defparameter *infix->prefix-rules*
  (mapcar #'expand-pat-match-abbrev
	  '(((x+ = y+) (= x y))
	    ((- x+)    (- x))
	    ((+ x+)    (+ x))
	    ((x+ + y+) (+ x y))
	    ((x+ - y+) (- x y))
	    ((d y+ / d x) (d y x))
	    ((Int y+ d x) (int y x))
	    ((x+ * y+) (* x y))
	    ((x+ / y+) (/ x y))
	    ((x+ ^ y+) (^ x y))))
  "A list of rules, ordered by precedence.")

(defstruct (rule (:type list)) pattern response)
(defstruct (exp (:type list)
		(:constructor mkexp (lhs op rhs)))
  op lhs rhs)

(defun exp-p (x) (consp x))

(defun exp-args (x) (rest x))

(defun prefix->infix (exp)
  (if (atom exp) exp
    (mapcar #'prefix->infix
	    (if (binary-exp-p exp)
	          (list (exp-lhs exp) (exp-op exp) (exp-rhs exp))
		  exp))))

(defun binary-exp-p (x)
  (and (exp-p x) (= (length (exp-args x)) 2)))

(defun ^ (x y) "Exponentiation" (expt x y))

(defun simplifier ()
  (loop
    (print 'simplifier>)
    (print (simp (read)))))

(defun simp (inf) (prefix->infix (simplify (infix->prefix inf))))

(defun simplify (exp)
  (if (atom exp) exp
    (simplify-exp (mapcar #'simplify exp))))

(defun evaluable (exp)
  (and (every #'numberp (exp-args exp))
       (or (member (exp-op exp) '(+ - * /))
	   (and (eq (exp-op exp) '^)
		(integerp (second (exp-args exp)))))))

(pat-match-abbrev 'n '(?is n numberp))
(pat-match-abbrev 'm '(?is m numberp))
(pat-match-abbrev 's '(?is s not-numberp))

(defun not-numberp (x) (not (numberp x)))

(defun simp-rule (rule)
  (let ((exp (infix->prefix rule)))
    (mkexp (expand-pat-match-abbrev (exp-lhs exp))
	   (exp-op exp) (exp-rhs exp))))

(defun simp-fn (op) (get op 'simp-fn))

(defun set-simp-fn (op fn) (setf (get op 'simp-fn) fn))

(defun simplify-exp (exp)
  (cond ((simplify-by-fn exp))
	((rule-based-translator exp *simplification-rules*
	   :rule-if #'exp-lhs :rule-then #'exp-rhs
	   :action #'(lambda (bindings response)
		       (simplify (sublis bindings response)))))
	((evaluable exp) (eval exp))
	(t exp)))

(defun simplify-by-fn (exp)
  (let* ((fn (simp-fn (exp-op exp)))
	 (result (if fn (funcall fn exp))))
    (if (null result)
	nil
	(simplify result))))

(defun factorize (exp)
  (let ((factors nil)
	  (constant 1))
  (labels
       ((fac (x n)
	   (cond
	     ((numberp x)
	      (setf constant (* constant (expt x n))))
	     ((starts-with x '*)
	      (fac (exp-lhs x) n)
	      (fac (exp-rhs x) n))
	     ((starts-with x '/)
	      (fac (exp-lhs x) n)
	      (fac (exp-rhs x) (- n)))
	     ((and (starts-with x '-) (length=1 (exp-args x)))
	      (setf constant (- constant))
	      (fac (exp-lhs x) n))
	     ((and (starts-with x '^) (numberp (exp-rhs x)))
	      (fac (exp-lhs x) (* n (exp-rhs x))))
	     (t (let ((factor (find x factors :key #'exp-lhs
				    :test #'equal)))
		  (if factor
		      (incf (exp-rhs factor) n)
		      (push `(^ , x , n) factors)))))))
      ;;Body factorize
      (fac exp 1)
      (case constant
	(0 '((~ 0 1)))
	(1 factors)
	(t `((^ , constant 1) .,factors))))))

(defun unfactorize (factors)
  (cond ((null factors) 1)
	((length=1 factors) (first factors))
	(t `(* ,(first factors) ,(unfactorize (rest factors))))))

(defun divide-factors (numer denom)
  (let ((result (mapcar #'copy-list numer)))
    (dolist (d denom)
      (let ((factor (find (exp-lhs d) result :key #'exp-lhs
			  :test #'equal)))
	(if factor
	    (decf (exp-rhs factor) (exp-rhs d))
	    (push `(^ , (exp-lhs d) , (- (exp-rhs d))) result))))
    (delete 0 result :key #'exp-rhs)))

(defun free-of (exp var)
  (not (find-anywhere var exp)))

(defun find-anywhere (item tree)
  (cond ((eql item tree) tree)
	((atom tree) nil)
	((find-anywhere item (first tree)))
	((find-anywhere item (rest tree)))))


(defun length=1 (x)
  "Is X a list of length 1?"
  (and (consp x) (null (rest x))))


(defun integrate (exp x)
  ;; First try some trivial cases
  (cond
    ((free-of exp x) `(* , exp x)) ;Int c dx = c * x
    ((starts-with exp '+) ;Int f + g =
     `(+ ,(integrate (exp-lhs exp) x) ;Int f + Int g
         ,(integrate (exp-rhs exp) x)))
    ((starts-with exp '-)
     (ecase (length (exp-args exp))
       (1 (integrate (exp-lhs exp) x)) ; Int - f = - Int f
       (2 `(- ,(integrate (exp-lhs exp) x) ;Int f - g =
	      ,(integrate (exp-rhs exp) x))))) ;Int f - Int g
    ;; Now move the constant factors to the left of the integral
    ((multiple-value-bind (const-factors x-factors)
	 (partition-if #'(lambda (factor) (free-of factor x))
		       (factorize exp))
       (simplify
	`(* ,(unfactorize const-factors)
	  ;; And try to integrate:
	    ,(cond ((null x-factors) x)
		   ((some #'(lambda (factor)
			      (deriv-divides factor x-factors x))
			  x-factors))
		   ;;<other methods here>
		   (t `(int? ,(unfactorize x-factors) ,x)))))))))


(defun partition-if (pred list)
  (let ((yes-list nil)
	(no-list nil))
    (dolist (item list)
      (if (funcall pred item)
	  (push item yes-list)
	  (push item no-list)))
    (values (nreverse yes-list) (nreverse no-list))))


(defun deriv-divides (factor factors x)
  (assert (starts-with factor '^))
  (let* ((u (exp-lhs factor)) ;factor = u^n
	 (n (exp-rhs factor))
	 (k (divide-factors
	     factors (factorize `(* ,factors ,(deriv u x))))))
    (cond ((free-of k x)
	   ;; Int k*u^n *du/dx dx = k*Int u^n du
	   ;;                     = k*u^(n+1)/(n+1) for n /= -1
	   ;;                     = k*log(u) for n = -1
	   (if (= n -1)
	       `(* ,(unfactorize k) (log ,u))
	       `(/ (* ,(unfactorize k) (^ ,u ,(+ n 1)))
		 ,(+ n 1))))
	  ((and (= n 1) (in-integral-table? u))
	   ;; Int y'*f(y) dx = Int f(y) dy
	   (let ((k2 (divide-factors
		      factors
		      (factorize `(* ,u ,(deriv (exp-lhs u) x))))))
	     (if (free-of k2 x)
		 `(* ,(integrate-from-table (exp-op u) (exp-lhs u))
		     ,(unfactorize k2))))))))

(defun deriv (y x) (simplify `(d ,y ,x)))


(defun integration-table (rules)
  (dolist (i-rule rules)
    (let ((rule (infix->prefix i-rule)))
      (setf (get (exp-op (exp-lhs (exp-lhs rule))) 'int)
	    rule))))


(defun in-integral-table? (exp)
  (and (exp-p exp) (get (exp-op exp) 'int)))


(defun integrate-from-table (op arg)
  (let ((rule (get op 'int)))
    (subst arg (exp-lhs (exp-lhs (exp-lhs rule))) (exp-rhs rule))))


(set-simp-fn 'Int #'(lambda (exp)
		      (integrate (exp-lhs exp) (exp-rhs exp))))

(set-simp-fn 'Int
	     #'(lambda (exp)
		 (unfactorize
		  (factorize
		   (integrate (exp-lhs exp) (exp-rhs exp))))))
