;; unless implements an unless macro with 'else' functionality

(defmacro unless 
	 "Evaluates test. If logical false, evaluates body.  Otherwise evaluates elsebody"
	([test then] `(unless ~test ~then nil))
	([test then else] `(if (not ~test) ~then ~else)))

(unless false (println "\nPASS1") )
(unless false (println "\nPASS2") (println "\nfail2"))
(unless true (println "\nfail3"))
(unless true (println "\nfail4") (println "\nPASS4"))
