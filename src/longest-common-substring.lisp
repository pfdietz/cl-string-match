(in-package :cl-string-match)

;;; Use suffix trees to find the longest common subsequence
;;; of two strings

(defun longest-common-substring (str1 str2 &key (mid-char #\#) (end-char #\$))
  (declare (type string str1 str2)
	   (type character mid-char end-char))
  (flet ((check-mid (s)
	   (when (find mid-char s)
	     (error "Separator character ~A occurs in ~A" mid-char s))))
    (check-mid str1)
    (check-mid str2))
  (flet ((check-end (s)
	   (when (find end-char s)
	     (error "Terminator character ~A occurs in ~A" mid-char s))))
    (check-end str1)
    (check-end str2))

  ;; Build the string str1 || mid-char || str-2 || end-char
  ;; Build a suffix tree for this string
  ;; Identify the node in the tree that represents a string in both the
  ;; str1 and str2 parts, and has maximum length.  This is the longest
  ;; common substring

  (let* ((len1 (length str1))
	 ;; (len2 (length str2))
	 (str (concatenate 'simple-string
			   str1 (string mid-char)
			   str2 (string end-char)))
	 (len-all (length str))
	 (stree (build-suffix-tree-ukkonen str)))
    (flet ((%which (node)
	     (let ((start (suffix-node-start node))
		   (end (suffix-node-end node)))
	       (assert (<= start end))
	       (cond
		 ((< end len1) 1)
		 ((> start (1+ len1)) 2)
		 (t 0))))
	   (%depth (n)
	     (loop while (suffix-node-parent n)
		do (setf n (suffix-node-parent n))
		sum 1)))
      ;; Actual computation of the LCSS
      (let ((best-chain (list (suffix-tree-root stree)))
	    (len 0))
	;; Initially, the LCSS is empty
	(labels ((%visit (n l chain)
		   ;; Visit returns an integer code indicating which
		   ;; of the two input strings occurred in the subtree
		   ;; 1 = first, 2 = second, 3 = both
		   (let* ((r 0)
			  (actual-end (min (1- len-all)
					   (suffix-node-end n)))
			  (node-start (suffix-node-start n))
			  (node-len (+ 1 l (- actual-end node-start)))
			  (next-chain (cons n chain)))
		     (declare (type (integer 0 3) r))
		     (loop for c in (suffix-node-children n)
			do (let ((cr (%visit c node-len next-chain)))
			     (declare (type (integer 0 3) cr))
			     (setf r (logior cr r))))
		     ;; Now visit this node
		     (setf r (logior r (%which n)))
		     (when (eql r 3)
		       ;; Both strings
		       (when (> node-len len)
			 (setf len node-len
			       best-chain next-chain)
			 ))
		     r)))
	  (%visit (suffix-tree-root stree) 0 nil)
	  (apply #'concatenate 'string
		 (cdr (nreverse (mapcar (lambda (n) (suffix-node-str stree n))
					best-chain))))
	  )))))

(defun random-string (n &optional (chars "abc"))
  (declare (type string chars))
  (let ((nc (length chars)))
    (assert (plusp nc))
    (coerce (loop repeat n collect (elt chars (random nc))) 'string)))

(defun longest-common-substring-test (n)
  (let ((s1 (random-string n))
	(s2 (random-string n)))
    (let ((lcs (longest-common-substring s1 s2)))
      (assert (search lcs s1))
      (assert (search lcs s2))
      (multiple-value-bind (lcs2 l2)
	  (dumb-lcss s1 s2)
	(assert (= l2 (length lcs2)))
	(assert (= l2 (length lcs))))
      (values lcs s1 s2))))

(defun dumb-lcss (s1 s2)
  (let ((len 0) (start 0) (end 0)
	(l1 (length s1)))
    (loop for i from 0 below l1
       do (loop for j from (+ i 1 len) to l1
	     do (let ((ss1 (subseq s1 i j)))
		  (when (and (> (- j i) len)
			     (search ss1 s2))
		    (setf start i end j len (- j i))))))
    (values (subseq s1 start end)
	    len)))
