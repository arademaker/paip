;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig
;;; Website http://norvig.com/paip.html

(in-package :utils)

(setf (symbol-function 'find-all-if) #'remove-if-not)

(defun one-of (set)
  "Pick one element of set, and make a list of it."
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defun mappend (fn the-list)
  "Apply fn t o each element of list and append the results."
  (apply 'append (mapcar fn the-list)))


(defun find-all (item sequence &rest keyword-args
		 &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item sequence
	     :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
	     :test (complement test) keyword-args)))


(defun flatten (the-list)
  "Append together elements (or lists) in the list."
  (mappend #'mklist the-list))

(defun mklist (x)
  "Return x if it is a list, otherwise (x)."
  (if (listp x)
      x
      (list x)))

(defun cross-product (fn xlist ylist)
  (mappend #'(lambda (y)
	       (mapcar #'(lambda (x) (funcall fn x y))
		       xlist))
	   ylist))


(defun combine-all (xlist ylist)
  (cross-product #'append xlist ylist))


(defun combinations (&rest lists)
  (if (car lists)
      (mapcan (lambda (inner-val)
		(mapcar (lambda (outer-val)
			  (cons outer-val inner-val))
			(car lists)))
	      (apply #'combinations (cdr lists)))
      (list nil)))


(defun insert (lst value index)
  (labels ((insert-aux (lst lst-value index left)
	   (if (zerop index)
	       (append left lst-value lst)
	       (insert-aux (cdr lst) lst-value (1- index)
	           (append left (list (car lst)))))))
    (insert-aux lst (list value) index nil)))


(defun eql-by-name-if-symbol (a b)
  (if (and (symbolp a) (symbolp b))
      (equal (symbol-name a)
	     (symbol-name b))
      (eql a b)))

(defun starts-with (list x)
  (and (consp list) (eql (first list) x)))

(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list)
       (eql-by-name-if-symbol (first list) x)))


;; (all-permutations '(1 1 2))
;; (all-permutations '(1 1))

;; (defun construct-candidates (part element)
;;   (loop for i from 0 to (length part)
;;        collect (insert (copy-list part) i element)))

;; (defun permutations-backtrack (part n newlist)
;;   (if (= n (length part))
;;       (list part)
;;       (let*((candidates (construct-candidates part (car newlist))))
;; 	(apply 'append (mapcar 'permutations-backtrack candidates
;; 			       (make-list n :initial-element n)
;; 			       (make-list n :initial-element (cdr newlist)))))))  

;; (defun generate-permutations (alist)
;;  (permutations-backtrack nil (length alist) alist))

;; resolver bugs
(defun permutations (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (print 'loop)
	   (loop for element in list
		 append (mapcar (lambda (l) (cons element l))
				(permutations (remove element list :count 1)))))))



(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))


(defun debug-on (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))


(defun debug-off (&rest ids)
  "Stop dbg on the ids.  With no ids, stop dbg altogether."
  (setf *dbg-ids* (if (null ids) nil
                      (set-difference *dbg-ids* ids))))


(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ "  " *debug-io*))
    (apply #'format *debug-io* format-string args)))


(defun string->list (str &optional (start 0) (alist nil))
  (multiple-value-bind (strn pos)
      (read-from-string str nil nil :start start)
    (if (and strn (<= pos (length str)))
	(string->list str pos (cons strn alist))
	(reverse alist))))


(defun interactive-interpreter (prompt transformer)
  (loop 
     (print prompt)
     (print (funcall transformer (read-line)))))

(defun interactive-interpreter (prompt transformer)
  (loop
    (handler-case
	(progn
	  (if (stringp prompt)
	      (print prompt)
	      (funcall prompt))
	  (force-output)
	  (print (funcall transformer (read-line))))
      (error (condition)
        (format t "~&;; Error ~a ignored, back to top level." condition)))))

(defun interactive-interpreter (&key (read #'read) (eval #'eval)
				  (print-prompt #'print) (prompt "> ")
				  (print-eval #'print))
  (loop
     (handler-case
	 (progn
	   (if (stringp prompt)
	       (funcall print-prompt prompt)
	       (funcall prompt))
	   (force-output)
	   (funcall print-eval (funcall eval (funcall read))))
       (error (condition)
	 (format t "~&;; Error ~a ignored, back to top level." condition)))))

(defun interactive-interpreter (&key (read #'read) (eval #'eval)
				  (print-prompt #'print) (prompt "> ")
				  (print-eval #'print) (exit #'equal)
				  (exit-symbol 'exit))
  (block out
    (loop
       (handler-case
	   (progn
	     (if (stringp prompt)
		 (funcall print-prompt prompt)
		 (funcall prompt))
	     (force-output)
	     (let* ((value 
		     (funcall eval (funcall read))))
	       (funcall print-eval value)
	       (if (funcall exit exit-symbol value)
		   (return-from out))))
       (error (condition)
	      (format t "~&;; Error ~a ignored, back to top level." condition))))))


(defun prompt-generator (&optional (num 0) (ctl-string "[~d] "))
  "Return a function that prints prompts like [1], [2], etc."
  #'(lambda () (format t ctl-string (incf num))))
