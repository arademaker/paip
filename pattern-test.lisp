
(in-package :pattern-test)

(deftest test-pat-match ()
 (check
  (equal (nth-value 1 (pat-match '(x = (?is ?n numberp)) '(x = 34))) '((?n . 34)))
  (equal (nth-value 1 (pat-match '(?x (?or < = >) ?y) '(3 < 4))) '((?y . 4) (?x . 3)))
  (multiple-value-bind (res binding)
      (pat-match '(teste ?x de (?* ?Y) funcionalidades da (?or pat-match segment-match))
		 '(teste parcial de algumas das funcionalidades da pat-match))
    (and res (equal binding '((?Y algumas das) (?X . parcial)))))
  (multiple-value-bind (res binding)
      (pat-match '(a (?* ?X) c d) '(a b c b c d))
    (and res (equal binding '((?X b c b)))))
  (multiple-value-bind (res binding)
      (pat-match '(teste de erro (?* ?X) segment match)
		 '(teste de erro da função segment-match))
    (equal `(,res ,binding) '(nil nil)))
  (multiple-value-bind (res binding)
      (pat-match '(teste da ultima (?* ?X) função)
		 '(teste da última saída possível da segment-match))
    (equal `(,res ,binding) '(nil nil)))))

(deftest test-pattern ()
  (combine-results
    (test-pat-match)))
