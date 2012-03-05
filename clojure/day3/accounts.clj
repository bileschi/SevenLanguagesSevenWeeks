; uses refs to hold a vector of accounts
; credit and debit functions alter account values in transactional fasion

(defn account
	"initializes one account"
	[]
	(ref 0))

(defn credit 
	"adds amount to one ref account"
	[account amnt]
	(dosync (alter account + amnt)))

(defn debit 
	"removes amount from one ref account"
	[account amnt]
	(credit account (- amnt)))

(defn account_list 
	"creates n accounts"
	[n]
	(vec (for [i (range 1 n)] (account))))

(defn credit_id
	"credits a specific account"
	[bank id amnt]
	(credit (bank id) amnt))

(defn debit_id
	"removes amount from account id"
	[bank id amnt]
	(debit (bank id) amnt))

; initialize accounts
(def bank_accounts (account_list 10))

; add $10 to account 3
