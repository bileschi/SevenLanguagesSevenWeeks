; ex: (file_to_lines "little_dict") ; returns a list containing 99 strings
; use to pull lines as strings into a list.  Each line is a string element 
(defun file_to_lines (filename)
  (let ((l ()))
    (with-open-file (stream filename)
      (do ((line (read-line stream nil)
		 (read-line stream nil)))
	  ((null line))
	(push line l)))
    (nreverse l)))

(defun hash_items_by_length (words)
  (let ((h (make-hash-table)))
    (loop for w in words
       do (let* ((k (length w)))
;	    (format t "[DEBUG] I want to add ~a[~a] to ~a~%" w k h)
	    (push (string-downcase w) (gethash k h))))
    h))

(defun hash-keys (h)
  (loop for k being the hash-keys of h collect k))

(defun print-hash (h)
  (loop for k being the hash-keys of h
       do (format t "[~a]  ~{~a, ~}~%" k (gethash k h))))

(defun main ()
  (print-hash (hash_items_by_length (file_to_lines "little_dict"))))