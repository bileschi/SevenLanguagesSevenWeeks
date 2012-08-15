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

; ex: (word-is-compound "butterscotch" my-dictionary)
; dictionary is hash from word-length to hash with words as keys
; if big-word is concatenation of two other valid words, return those two valid words
; in the example, return '("butter", "scotch")
(defun word-is-compound (big-word dict)
  (let ((big-l (length big-word)))
    (loop for left-l from 1 to (- big-l 1) do
	 (let ((left-word (subseq big-word 0 left-l))
	       (right-word (subseq big-word left-l)))
	   (if (and (dict-contains left-word dict)
		    (dict-contains right-word dict))
	       (return-from word-is-compound (list left-word right-word)))))))

; ex: (main)
; returns the longest word it can find that is made up of a concatenation of two other words
; returns in the form '(longword (leftword rightword))
; in the event of a tie, it returns one of the tied elements
(defun main ()
  (let ((dict (hash-items-by-length (file-to-lines "dict"))))
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
		    (return-from main (list compound-word? solution?)))))))))


