;; collectiontype.clj

(defn collection-type
	"returns :list :map or :vector depending on input type."
	[col]
	(if (list? col) :list 
		(if (map? col) :map
			(if (vector? col) :vector :not-list-map-or-vector)
		)
	)
)


;; a simple test of the above:

;; user=> (collection-type ())
;; :list
;; user=> (collection-type [])
;; :vector
;; user=> (collection-type {})
;; :map
;; user=> (collection-type collection-type)
;; :not-list-map-or-vector
