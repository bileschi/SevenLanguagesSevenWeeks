;; big.clj


(defn big
	"returns true if a string st is longer than n characters"
	[st n]
	(> (count st) n)
)