
(in-package :search-test)

(deftest test-depth-first-search ()
  (check
    (equal (depth-first-search 1 (is 16) (finite-binary-tree 15)) nil)
    (equal (depth-first-search 1 (is 13) (finite-binary-tree 15)) 13)
    (equal (depth-first-search 1 (is 12) (finite-binary-tree 15)) 12)))

(deftest test-breadth-first-search ()
  (check
    (equal (breadth-first-search 1 (is 12) 'binary-tree) 12)))

(deftest test-best-first-search ()
  (check
    (equal (best-first-search 1 (is 12) 'binary-tree (diff 12)) 12)))

(deftest test-search-n ()
  (check
    (equal '(125 255 15)
	   (search::search-n 1 3
			     (lambda (n) (equal 0 (mod n 5)))
			     #'binary-tree
			     (search::price-is-right 300) 5))))

;;(deftest test-search-gps ()
;;  (check
;;    (equal (search-gps '((c on a) (a on table) (b on table) (space on c) (space on b) (space on table))
;;		       '((b on c) (a on b))) '((GPS-2::EXECUTING (MOVE C FROM A TO TABLE))
;;					       (GPS-2::EXECUTING (MOVE B FROM TABLE TO C))
;;					       (GPS-2::EXECUTING (MOVE A FROM TABLE TO B))))))

(deftest test-search ()
  (combine-results
    (test-depth-first-search)
    (test-breadth-first-search)
    (test-best-first-search)))
