;;  Author: Alexandre Rademaker
;;
;; For info why the dependencies file is necessary, read
;; http://weitz.de/packages.html

(asdf:defsystem #:paip
    :serial t
    :components ((:file "packages")
		 (:file "utils"             :depends-on ("packages"))
		 (:file "tests-framework"   :depends-on ("utils"))
		 (:file "sentence-1"        :depends-on ("utils"))
		 (:file "sentence-2"        :depends-on ("utils"))
		 (:file "gps-1"             :depends-on ("utils"))
		 (:file "gps-2"             :depends-on ("utils"))
		 (:file "gps-blocks"        :depends-on ("gps-2"))
		 (:file "gps-monkey"        :depends-on ("gps-2"))
		 (:file "gps-maze"          :depends-on ("gps-2"))
		 (:file "gps-school"        :depends-on ("gps-2"))
		 (:file "pattern"           :depends-on ("utils"))
		 (:file "pattern-test"      :depends-on ("pattern" "tests-framework"))
		 (:file "eliza"             :depends-on ("pattern"))
		 (:file "eliza-english"     :depends-on ("eliza"))
		 (:file "eliza-portuguese"  :depends-on ("eliza"))
		 (:file "search"            :depends-on ("utils"))
		 (:file "search-test"       :depends-on ("search"  "tests-framework"))
		 (:file "gps-search"        :depends-on ("search"))
		 (:file "student"           :depends-on ("pattern"))
		 (:file "clos"              :depends-on ("utils"))
		 (:file "math-symbolic"     :depends-on ("pattern"))
		 (:file "math-rules"        :depends-on ("math-symbolic"))
		 (:file "battleship"        :depends-on ("utils"))))
