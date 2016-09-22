;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

(in-package :eliza)

(defparameter +no-bindings+ '((T . T)))
(defparameter +fail+ nil)

(defun rule-pattern (rule) (first rule))
(defun rule-responses (rule) (rest rule))

(defparameter *rules* nil)

(defun use-eliza-rules (input &key (rules *rules*) (preproc #'identity))
  "Find some rule with which to transform the input."
  (some #'(lambda (rule)
            (let ((result (pat-match (rule-pattern rule) input)))
              (if (not (eq result +fail+))
                  (sublis (funcall preproc result)
                          (random-elt (rule-responses rule))))))
        rules))

(defun use-eliza-rules (input &key (rules *rules*) (preproc #'identity))
  (rule-based-translator input *rules*
			 :action #'(lambda (bindings responses)
				     (sublis (switch-viewpoint bindings)
					     (random-elt responses)))))

(defun eliza (rules preproc)
  "Respond to user input using pattern matching rules."
  (block out
    (loop
     (print 'eliza-prompt>)
     (let* ((answer 
	     (flatten (use-eliza-rules (string->list (read-line)) :rules rules 
				       :preproc preproc))))
       (format t "~{~A ~}" answer)
       (if (member 'exit answer :test #'utils:eql-by-name-if-symbol)
	   (return-from out))))))
		   
(defun eliza (rules preproc)
  "Respond to user input using pattern matching rules."
  (interactive-interpreter
   :read #'read-line 
   :eval #'(lambda(x) 
	     (flatten (use-eliza-rules (string->list x) :rules rules 
				       :preproc preproc)))
   :print-prompt #'(lambda(x) (format t "~A" x))
   :print-eval #'(lambda(x) (format t "~{~A ~}~%" x))
   :exit #'(lambda(x y) 
	     (member x y :test #'utils:eql-by-name-if-symbol))
   :prompt (prompt-generator 0 "eliza [~d] > ")))
