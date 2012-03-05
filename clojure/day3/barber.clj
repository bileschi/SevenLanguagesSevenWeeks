; http://en.wikipedia.org/wiki/Sleeping_barber_problem

;;;;;;;;;;; Definition ;;;;;;;;;;;;;;;;;;;;;;

;A barber shop takes customers.
;Customers arrive at random intervals, from 10 to 30 milliseconds.
;The barber shop has three chairs in the waiting room.
;The barber shop has one barber and one barber chair.
;When the barber's chair is empty, a customer sits in the chair, wakes up the barber, and gets a haircut.
;If the chairs are occupied, all new customers will turn away.
;Haircuts take 20 milliseconds.
;After a customer receives a haircut, he gets up and leaves.

;;;;;;;;;;; Implementation ;;;;;;;;;;;;;;;;;;;


(def barber (agent {:name "vince" :busy false}))
(def barber_shop (agent [])) ; indicates the 3 seats

(defn arrive
	[shop, customer_id]
	(do 
		(Thread/sleep (+ 10 (rand-int 20)))
		(if (< (count shop) 3) ; if room in shop
			(do (println "ding-dong") (conj shop customer_id)) ; add a customer
			(do (println "harumph") (shop))) ; no customer.
	)
)

(for [customer_id (range 200)]
	(send barber_shop arrive customer_id)
)

