
(in-package :gps-test)

(defparameter *school-ops*
  (mapcar #'gps-2::convert-op
	  (list
	   (make-op :action 'drive-son-to-school
		    :preconds '(son-at-home car-works)
		    :add-list '(son-at-school)
		    :del-list '(son-at-home))
	   (make-op :action 'shop-installs-battery
		    :preconds '(car-needs-battery shop-knows-problem shop-has-money)
		    :add-list '(car-works))
	   (make-op :action 'tell-shop-problem
		    :preconds '(in-communication-with-shop)
		    :add-list '(shop-knows-problem))
	   (make-op :action 'telephone-shop
		    :preconds '(know-phone-number)
		    :add-list '(in-communication-with-shop))
	   (make-op :action 'look-up-number
		    :preconds '(have-phone-book)
		    :add-list '(know-phone-number))
	   (make-op :action 'give-shop-money
		    :preconds '(have-money)
		    :add-list '(shop-has-money)
		    :del-list '(have-money))
	   (make-op :action 'ask-phone-number
		    :preconds '(in-communication-with-shop)
		    :add-list '(know-phone-number)))))
