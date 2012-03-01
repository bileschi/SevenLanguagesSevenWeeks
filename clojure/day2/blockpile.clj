;; testing defprotocol and defrecord
;; with a silly, record with that describes a stack of blocks
;; block piles can only be 3 blocks high and minimum of 0 blocks

(defprotocol DiscreetPile
	(add-one [c])
	(remove-one [c]))

(defrecord BlockPile [height]
	DiscreetPile 
	(add-one [_] (if (>= height  3) (BlockPile. 3) (BlockPile. (+ height 1))))
	(remove-one [_] (if (<= height 0) (BlockPile. 0) (BlockPile. (- height 1))))
	Object
	(toString [this] (str "[" + height + "]")))

(def c (BlockPile. 0))
(add-one c)
(add-one (add-one c))
(add-one (add-one (add-one c)))
(add-one (add-one (add-one (add-one c))))
(def d (BlockPile. 3))
(remove-one d)
(remove-one (remove-one d))
(remove-one (remove-one (remove-one d)))
(remove-one (remove-one (remove-one (remove-one d))))
