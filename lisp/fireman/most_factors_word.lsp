; ex: (file-to-lines "little_dict") ; returns a list containing 99 strings
; use to pull lines as strings into a list.  Each line is a string element 
(defun file-to-lines (filename)
  (let ((l ()))
    (with-open-file (stream filename)
      (do ((line (read-line stream nil)
		 (read-line stream nil)))
	  ((null line))
	(push line l)))
    (nreverse l)))

; ex: (hash-items-by-length '("a", "A", "at", "attack")) returns a hashmap
; with keys (1, 2, 6).  Each value is also hashmap, keyed by the words and with
; irrelevant values.  (gethash 1 *MYHASH*) will have ("a") and ("A") as keys.
;  Builds a hashmap of length to (hashmap of word to true)
(defun hash-items-by-length (words)
  (let ((h (make-hash-table :test 'equal)))
    (loop for w in words
       do (let* ((k (length w))
		 (words-of-length-k (gethash k h)))	    
;	    (format t "[DEBUG] I want to add ~a[~a] to ~a~%" w k h)
	    (unless words-of-length-k ; initialize subhash if necc
	      (setf (gethash k h) (make-hash-table :test 'equal))
	      (setf words-of-length-k (gethash k h)))
	    (setf (gethash w words-of-length-k) t) ; store with (key, val) == (word, TRUE)
	    ))
    h))

; returns a list of the keys of a hashmap
(defun hash-keys (h)
  (loop for k being the hash-keys of h collect k))

; ex (dict-contains "agar" words-starting-with-a) ; should return t
; returns true iff the dictionary contains the word
; dict is a hash from wordlength to a hash from word to true.
(defun dict-contains (w dict)
  (let ((l (length w)))
    (when (gethash l dict)
      (gethash w (gethash l dict)))))

; helper to print-hash-of-hash
(defun print-sub-hash (sh)
  (loop for w being the hash-keys of sh
        do (format t "~S, " w)))
  

;helpful printing of the top level hashmap
(defun print-hash-of-hash (h)
  (loop for l being the hash-keys of h
     using (hash-value sub-hash)
        do (progn 
	     (format t "[~S]" l)
	     (print-sub-hash sub-hash)
	     (format t "~%"))))

; ex: (wordprefixes "characters" dict) -> ("cha", "char", "character")
; returns a list of proper prefixes of word which are words in dict
; list is sorted shortest to longest prefix
(defun wordprefixes (word dict &key (min-prefix 3))
  (let ((L (length word))
	  prefix-words)
    (loop for i from min-prefix to (- L 1) do
	 (let ((prefix (subseq word 0 i)))
	   (when (dict-contains prefix dict)
	     (push prefix prefix-words))))
    (reverse prefix-words)))

(defvar dict (hash-items-by-length (file-to-lines "dict")))

; toy function returns the word with the most prefixes
; which are also words.
(defun most-prefixes (allwords dict)
  (let ((most 0)
	subs)
    (loop for word in allwords do
       (let ((prefixes (wordprefixes word dict)))
	 (when (> (length prefixes) most)
	   (print word)
	   (setf most (length prefixes))
	   (setf subs prefixes))))
    (return-from most-prefixes (list most subs))))

; toy function returns the word with the most prefixes
; which are also words.
(defun most-factors (allwords dict)
  (let ((most 0)
	subs)
    (loop for word in allwords do
       (let ((factors (factorize-word word dict :LB most)))
	 (when (> (length factors) most)
	   (format t "~S : ~S~%" word factors)
	   (setf most (length factors))
	   (setf subs factors))))
    (return-from most-factors (list most subs))))

; (defmacro dbformat (&rest Args)  `(format ,@Args))
(defmacro dbformat (&rest Args) '() )

; returns a the longest list of words which when concatenated 
; produce the argument word.  Returns NIL if word can not be factored
; into more than LB (lower bound) factors.  All factors must be at least
; min-sub-len letters.
(defun factorize-word (word dict &key (LB 0) (min-sub-len 3) (depth 0))
  (if (null word) (return-from factorize-word ()))
  (let* ((L (length word))
	  ; UB is the upper bound.  the largest number of factors of word
	 (UB (floor (/ L min-sub-len)))
	 (max-n-factors 0)
	 max-factors)
    (if (> LB UB) (return-from factorize-word ())) ; LB can not be reached.
    (let ((prefixes (wordprefixes word dict)))
      ; loop in order of shortest prefix first.
      ; this order is essential for early termination condition
      (loop for i from 0 to (+ 1 depth) do (format t " "))
      (dbformat t "prefixes of ~s are ~S~%" word prefixes)
     (loop for prefix in prefixes do
	   (loop for i from 0 to depth do (format t " "))
	   (dbformat t "trying prefix ~s~%" prefix)
	   (let* ((prefix-L (length prefix))
		  (suffix-L (- L prefix-L))
		  (UB-suffix (floor (/ suffix-L min-sub-len)))
		  (suffix (subseq word prefix-L L)))
	     ; if suffix too short to satisfy LB we can stop now.
	     ; an even *shorter* suffix will also fail.
	     (if (< (+ 1 UB-suffix) LB)
		 (return-from factorize-word max-factors))
	     (let ((suffix-factors (factorize-word 
				    suffix 
				    dict 
				    :LB (- LB 1) 
				    :MIN-SUB-LEN min-sub-len
				    :DEPTH (+ 1 depth))))
	       (if suffix-factors 
		   (when (>= (length suffix-factors) max-n-factors)
		     (setf max-n-factors (+ 1 (length suffix-factors)))
		     (setf max-factors (append (list prefix) suffix-factors))
		     (setf LB (+ max-n-factors 1))
		     (loop for i from 0 to (+ 1 depth) do (format t " "))
		     (dbformat t "factors are ~S~%" max-factors))
		   (progn
		     (loop for i from 0 to (+ 1 depth) do (format t " "))
		     (dbformat t "no suffix factors for ~S~%" suffix)
		     )
		   )))))
    ; if a one word factor is ok and we havn't done better and this IS a word
    ; return a factorization of '(thisword)
    (loop for i from 0 to (+ 1 depth) do (format t " "))
    (dbformat t "~a:~a:~b~%" (<= LB 1) (< max-n-factors) (dict-contains word dict))
    (when (and (<= LB 1)
	       (< max-n-factors 1)
	       (dict-contains word dict))
      (loop for i from 0 to (+ 1 depth) do (format t " "))
      (dbformat t "factors are ~S~%" (list word))
      (return-from factorize-word (list word)))
    max-factors))




(defun main ()
    (search-compound "dict"))

; ex: (word-is-compound "butterscotch" my-dictionary)
; dictionary is hash from word-length to hash with words as keys
; if big-word is concatenation of two other valid words, return those two valid words
; in the example, return '("butter", "scotch")
;
; DEPRICATE AND DELETE
(defun word-is-compound (big-word dict)
  (let ((big-l (length big-word)))
    (loop for left-l from 1 to (- big-l 1) do
	 (let ((left-word (subseq big-word 0 left-l))
	       (right-word (subseq big-word left-l)))
	   (if (and (dict-contains left-word dict)
		    (dict-contains right-word dict))
	       (return-from word-is-compound (list left-word right-word)))))))

; ex: (search-compound "dict")
; returns the longest word it can find that is made up of a concatenation 
; of two other words
; returns in the form '(longword (leftword rightword))
; in the event of a tie, it returns one of the tied elements
;
; DEPRICATE AND DELETE
(defun search-compound (filename)
  (let ((dict (hash-items-by-length (file-to-lines filename))))
    (let ((lengths (hash-keys dict)))
      ; loop for l in reverse lengths, biggest first
      (setf lengths (reverse lengths))
      (loop for l in lengths do
	   ; loop over the words of this size
	   (loop for compound-word? being the hash-key of (gethash l dict)
		do
		; check if word is actually compound
		(let ((solution? (word-is-compound compound-word? dict)))
		  (if solution?
		    (return-from search-compound 
		      (list compound-word? solution?)))))))))
