;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig
;;; Website http://norvig.com/paip.html

(in-package :chapter-1)

(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))


(defun generate (phrase)
  "Explit tests but call rewrites twice!"
  (cond
    ((listp phrase)
     (mappend #'generate phrase))
    ((rewrites phrase)
     (generate (random-elt (rewrites phrase))))
    (t (list phrase))))

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (if (listp phrase)
      (mappend 'generate phrase)
      (let ((choices (rewrites phrase)))
	(if (null choices)
	    (list phrase)
	    (generate (random-elt choices))))))


(defun generate-tree (phrase)
  (cond 
    ((list phrase)
     (mapcar #'generate-tree phrase))
    ((rewrites phrase)
     (cons phrase
	   (generate-tree (random-elt (rewrites phrase)))))
    (t (list phrase))))


(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (article noun))
    (verb-phrase -> (verb noun-phrase))
    (article -> the a)
    (noun -> man ball woman table)
    (verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")
  
(defparameter *bigger-gramar*
  '((sentence -> (noun-phrase verb-phrase)
     (noun-phrase -> (article adj* noun pp*) (name) (pronoun))
     (verb-phrase -> (verb noun-phrase pp*))
     (pp* -> ()(pp pp*))
     (adj* -> () (adj adj*))
     (pp -> (prep noun-phrase))
     (prep -> in by to on with)
     (adj -> big little blue green adiabatic)
     (article -> the a)
     (name -> Pat Kim Lee Terry Robin)
     (noun -> man ball woman table)
     (verb -> hit took saw liked)
     (pronoun -> he she it these those that))))

(defparameter *grammar-pt*
  '((sentence          -> (noun-phrase verb-phrase))
    (noun-phrase       -> masculino feminino)
    (masculino         -> (article-masculino noun-masculino))
    (feminino          -> (article-feminino noun-feminino))
    (verb-phrase       -> (verb noun-phrase))
    (article-masculino -> o um)
    (noun-masculino    -> homem menino rapaz)
    (article-feminino  -> a uma)
    (noun-feminino     -> mulher menina garota)
    (verb              -> abraçou empurrou)))

(defvar *grammar* *grammar-pt*)

