;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig
;;; Website http://norvig.com/paip.html

(in-package :chapter-1)

(defun sentence ()    (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (article) (noun)))
(defun verb-phrase () (append (verb) (noun-phrase)))
(defun article ()     (one-of '(the a)))
(defun noun ()        (one-of '(man ball woman table)))
(defun verb ()        (one-of '(hit took saw liked)))


(defun adj* ()
  "problema!"
  (one-of '(nil (append (adj) (adj*)))))

(defun adj* ()
  "problema!"
  (one-of (list nil (append (adj) (adj*)))))

(defun adj* ()
  (if (= (random 2) 0)
      nil
      (append (adj) (adj*))))

(defun pp* ()
  (if (random-elt '(t nil))
      (append (pp) (pp*))
      nil))

(defun pp ()
  (append (prep) (noun-phrase)))

(defun adj ()
  (one-of '(big little blue green adiabatic)))

(defun prep ()
  (one-of '(to in by with on)))

(defun noun-phrase ()
  (append (article) (adj*) (noun) (pp*)))



