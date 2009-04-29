;; This file is part of Sheeple

;; message-dispatch.lisp
;;
;; Message execution and dispatch
;;
;; TODO
;; * memoize message dispatch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(declaim (optimize (safety 0) (speed 3) (debug 0)))
(defun primary-message-p (message)
  (null (message-qualifiers message)))

(defun before-message-p (message)
  (when (member :before (message-qualifiers message))
    t))

(defun after-message-p (message)
  (when (member :after (message-qualifiers message))
    t))

(defun around-message-p (message)
  (when (member :around (message-qualifiers message))
    t))

(defun apply-buzzword (buzzword args)
  (let ((messages (find-applicable-messages buzzword args)))
    (apply-messages messages args)))

(defstruct (cache (:type vector))
  name
  around
  primary
  before
  after
  messages)

(defun apply-messages (cache args)
  (let ((messages (cache-messages cache))
	(around (cache-around cache))
	(primaries (cache-primary cache)))
    (when (null primaries)
      (let ((name (cache-name cache)))
	(error 'no-primary-messages
	       :format-control 
	       "There are no primary messages for buzzword ~A When called with args:~%~S"
	       :format-args (list name args))))
    (if around
	(apply-message (car around) args (remove around messages))
    	(let ((befores (cache-before cache))
	      (afters (cache-after cache)))
	  (when befores
	    (dolist (before befores)
	      (apply-message before args nil)))
	  (multiple-value-prog1
	      (apply-message (car primaries) args (cdr primaries))
	    (when afters
	      (dolist (after (reverse afters))
		(apply-message after args nil))))))))

(defun create-message-cache (buzzword messages)
  (make-cache
   :name (buzzword-name buzzword)
   :messages messages
   :primary (remove-if-not #'primary-message-p messages)
   :around (remove-if-not #'around-message-p messages)
   :before (remove-if-not #'before-message-p messages)
   :after (remove-if-not #'after-message-p messages)))

(defun apply-message (message args next-messages)
  (let ((function (message-function message)))
    (funcall (the function function) args next-messages)))

(defun find-applicable-messages (buzzword args &key (errorp t))
  (declare (list args))
  (declare (buzzword buzzword))
  (let* ((relevant-args-length (the fixnum (arg-info-number-required (buzzword-arg-info buzzword))))
	 (memo-entry (fetch-memo-vector-entry args buzzword relevant-args-length)))
    (or memo-entry
	memo-entry
	(let* ((relevant-args (the list (subseq args 0 relevant-args-length)))
	       (new-msg-list (%find-applicable-messages buzzword 
							relevant-args
							:errorp errorp)))
	  (memoize-message-dispatch buzzword relevant-args new-msg-list)))))

(defun fetch-memo-vector-entry (args buzzword relevant-args-length)
  (let* ((memo-vector (buzzword-memo-vector buzzword))
	 (orig-index (mod (the fixnum (sheep-id (sheepify (car args))))
			  (length memo-vector))))
    (declare (vector memo-vector))
    (declare (fixnum orig-index))
    (let ((attempt (elt (the (not simple-array) memo-vector) orig-index)))
      (if (desired-vector-entry-p args attempt relevant-args-length)
	  (vector-entry-msg-cache attempt)
	  (progn
	    (loop for entry across memo-vector
	       do (when (desired-vector-entry-p args entry relevant-args-length)
		    (return-from fetch-memo-vector-entry (vector-entry-msg-cache entry))))
	    nil)))))

(defun desired-vector-entry-p (args vector-entry relevant-args-length)
  (when (vectorp vector-entry)
    (let ((vector-args (vector-entry-args vector-entry)))
      (loop
	 for i upto relevant-args-length
	 for v-arg in vector-args
	 for arg in args
	 do (when (not (eql v-arg arg))
	      (return-from desired-vector-entry-p nil)))
      t)))

(defstruct (vector-entry (:type vector))
  args
  msg-cache)

(defun memoize-message-dispatch (buzzword args msg-list)
  (let ((msg-cache (create-message-cache buzzword msg-list))
	(maybe-index (mod (the fixnum (sheep-id (sheepify (car args))))
			  (length (the vector (buzzword-memo-vector buzzword))))))
    (add-entry-to-buzzword msg-cache buzzword args maybe-index)
    msg-cache))

(defun add-entry-to-buzzword (cache buzzword args index)
  (let ((memo-vector (buzzword-memo-vector buzzword)))
    (declare (fixnum index))
    (declare (simple-array memo-vector))
    (loop for i from index
       do (progn
	    (when (>= i (length memo-vector))
	      (adjust-array memo-vector (+ (length memo-vector) 8)))
	    (when (eql (elt (the (not string) memo-vector) i) 0)
	      (setf (elt memo-vector index) (make-vector-entry 
					     :args args
					     :msg-cache cache))
	      (loop-finish))))))

(defun %find-applicable-messages  (buzzword args &key (errorp t))
  "Returns the most specific message using BUZZWORD and ARGS."
  (let ((selector (buzzword-name buzzword))
	(n (length (the list args)))
	(discovered-messages nil)
	(contained-applicable-messages nil))
    (declare (list discovered-messages contained-applicable-messages))
    (loop 
       for arg in args
       for index upto (1- n)
       do (let* ((arg (if (sheep-p arg)
			  arg
			  (sheepify arg)))
		 (curr-sheep-list (sheep-hierarchy-list arg)))
	    (loop
	       for curr-sheep in curr-sheep-list
	       for hierarchy-position upto (1- (length curr-sheep-list))
	       do (dolist (role (sheep-direct-roles curr-sheep))
		    (when (and (equal selector (role-name role)) ;(eql buzzword (role-buzzword role))
			       (= (the fixnum index) (the fixnum (role-position role))))
		      (let ((curr-message (role-message-pointer role)))
			(when (= n (length (the list (message-specialized-portion curr-message))))
			  (when (not (member curr-message
					     discovered-messages
					     :key #'message-container-message))
			    (pushnew (the vector (contain-message curr-message))
				     discovered-messages))
			  (let ((contained-message (find curr-message
							 discovered-messages
							 :key #'message-container-message)))
			    (setf (elt (message-container-rank contained-message) index) 
				  hierarchy-position)
			    (when (fully-specified-p (message-container-rank contained-message))
			      (pushnew contained-message contained-applicable-messages :test #'equalp))))))))))
    (if contained-applicable-messages
	(unbox-messages (sort-applicable-messages contained-applicable-messages))
	(when errorp
	  (error 'no-applicable-messages
		 :format-control
		 "There are no applicable messages for buzzword ~A when called with args:~%~S"
		 :format-args (list selector args))))))

(defun unbox-messages (messages)
  (mapcar #'message-container-message messages))

(defun sort-applicable-messages (message-list &key (rank-key #'<))
  (sort message-list rank-key
	:key (lambda (contained-message)
	       (calculate-rank-score (message-container-rank contained-message)))))

(defun contain-message (message)
  (make-message-container
   :message message
   :rank (make-array (length (the list (message-specialized-portion message)))
		     :initial-element nil)))

(defstruct (message-container (:type vector))
  message
  rank)

(defun fully-specified-p (rank)
  (loop for item across rank
     do (when (eql item nil)
	  (return-from fully-specified-p nil)))
  t)

(defun calculate-rank-score (rank)
  (declare (simple-array rank))
  (let ((total 0))
    (declare (fixnum total))
    (loop for item across rank
       do (when (numberp item)
	    (incf total (the fixnum item))))
    total))

(defun message-specialized-portion (msg)
  (parse-lambda-list (message-lambda-list msg)))
