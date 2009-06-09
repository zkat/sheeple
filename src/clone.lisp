;; clone.lisp
;;
;; Contains the clone macro and utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(defun canonize-sheeple (sheeple)
  `(list ,@(mapcar #'canonize-sheep sheeple)))

(defun canonize-sheep (sheep)
  `(confirm-sheep ,sheep))

(defun confirm-sheep (sheep)
  sheep)

(defun canonize-properties (properties)
  `(list ,@(mapcar #'canonize-property properties)))

(defun canonize-properties* (properties)
  `(list ,@(mapcar #'canonize-property* properties)))

(defun canonize-property* (property)
  "This version is fancier and adds readers/writers automatically"
  (if (symbolp property)
      `(list :name ',property)
      (let ((name (car property))
	    (value (cadr property))
            (readers nil)
            (writers nil)
	    (lockedp nil)
	    (cloneform *secret-unbound-value*)
            (other-options nil)
	    (no-reader-p nil)
	    (no-writer-p nil))
	(do ((olist (cddr property) (cddr olist)))
            ((null olist))
	  (case (car olist)
            (:reader
	     (cond (no-reader-p
		    (error "You said you didn't want a reader, but now you want one? Make up your mind."))
		   ((null (cadr olist))
		    (if (null readers)
			(setf no-reader-p t)
			(error "You already defined a reader, but now you say you don't want one? Make up your mind.")))
		   (t
		    (pushnew (cadr olist) readers))))
            (:writer
	     (cond (no-writer-p
		    (error "You said you didn't want a writer, but now you want one? Make up your mind."))
		   ((null (cadr olist))
		    (if (null writers)
			(setf no-writer-p t)
			(error "You already defined a writer, but now you say you don't want one? Make up your mind.")))
		   (t
		    (pushnew (cadr olist) writers))))
            ((:manipulator :accessor)
	     (cond ((or no-reader-p no-writer-p)
		    (error "You said you didn't want a reader or a writer, but now you want one? Make up your mind."))
		   ((null (cadr olist))
		    (if (and (null writers) (null readers))
			(progn
			 (setf no-reader-p t)
			 (setf no-writer-p t))
			(error "You already defined a reader or writer, but now you say you don't want any of them? Make up your mind.")))
		   (t
		    (pushnew (cadr olist) readers)
		    (pushnew `(setf ,(cadr olist)) writers))))
	    (:cloneform
	     (setf cloneform (cadr olist)))
	    ((:locked-p :lockedp)
	     (setf lockedp (cadr olist)))
	    (otherwise 
             (pushnew (cadr olist) other-options)
             (pushnew (car olist) other-options))))
	(unless (or readers no-reader-p)
	  (pushnew name readers))
	(unless (or writers no-writer-p)
	  (pushnew `(setf ,name) writers))
	(if other-options
	    (error "Invalid property option(s)")
	    `(list
	      :name ',name
	      :value ,value
	      ,@(when (not (eql cloneform *secret-unbound-value*))
		      `(:cloneform ',cloneform :clonefunction (lambda () ,cloneform)))
	      ,@(when readers `(:readers ',readers))
	      ,@(when writers `(:writers ',writers)))))))

(defun canonize-property (property)
  (if (symbolp property)
      `(list :name ',property)
      (let ((name (car property))
	    (value (cadr property))
            (readers nil)
            (writers nil)
	    (lockedp nil)
	    (cloneform *secret-unbound-value*)
            (other-options nil)
	    (no-reader-p nil)
	    (no-writer-p nil))
	(do ((olist (cddr property) (cddr olist)))
            ((null olist))
	  (case (car olist)
            (:reader
	     (cond (no-reader-p
		    (error "You said you didn't want a reader, but now you want one? Make up your mind."))
		   ((null (cadr olist))
		    (if (null readers)
			(setf no-reader-p t)
			(error "You already defined a reader, but now you say you don't want one? Make up your mind.")))
		   (t
		    (pushnew (cadr olist) readers))))
            (:writer
	     (cond (no-writer-p
		    (error "You said you didn't want a writer, but now you want one? Make up your mind."))
		   ((null (cadr olist))
		    (if (null writers)
			(setf no-writer-p t)
			(error "You already defined a writer, but now you say you don't want one? Make up your mind.")))
		   (t
		    (pushnew (cadr olist) writers))))
            ((:manipulator :accessor)
	     (cond ((or no-reader-p no-writer-p)
		    (error "You said you didn't want a reader or a writer, but now you want one? Make up your mind."))
		   ((null (cadr olist))
		    (if (and (null writers) (null readers))
			(progn
			 (setf no-reader-p t)
			 (setf no-writer-p t))
			(error "You already defined a reader or writer, but now you say you don't want any of them? Make up your mind.")))
		   (t
		    (pushnew (cadr olist) readers)
		    (pushnew `(setf ,(cadr olist)) writers))))
	    (:cloneform
	     (setf cloneform (cadr olist)))
	    ((:locked-p :lockedp)
	     (setf lockedp (cadr olist)))
	    (otherwise 
             (pushnew (cadr olist) other-options)
             (pushnew (car olist) other-options))))
	(if other-options
	    (error "Invalid property option(s)")
	    `(list
	      :name ',name
	      :value ,value
	      ,@(when (not (eql cloneform *secret-unbound-value*))
		      `(:cloneform ',cloneform :clonefunction (lambda () ,cloneform)))
	      ,@(when readers `(:readers ',readers))
	      ,@(when writers `(:writers ',writers)))))))

(defun canonize-clone-options (options)
  (mapappend #'canonize-clone-option options))

(defun canonize-clone-option (option)
  (list `',(car option) (cadr option)))

(defmacro clone (sheeple properties &rest options)
  "Standard sheep-generation macro"
  `(spawn-sheep
    ,(canonize-sheeple sheeple)
    :properties ,(canonize-properties properties)
    ,@(canonize-clone-options options)))

(defmacro clone* (sheeple properties &rest options)
  "Standard sheep-generation macro. This variant auto-generates accessors."
  `(spawn-sheep
    ,(canonize-sheeple sheeple)
    :properties ,(canonize-properties* properties)
    ,@(canonize-clone-options options)))

(defmacro defsheep (name sheeple properties &rest options)
  (if (boundp name)
      `(progn
         (defvar ,name)
         (let ((sheep (replace-or-reinitialize-sheep 
                       ,name 
                       ,(canonize-sheeple sheeple)
                       ,(canonize-properties* properties) 
                       ,@(canonize-clone-options options))))
           (unless (sheep-nickname sheep)
             (setf (sheep-nickname sheep) ',name))
           (setf ,name sheep)
           ',name))
      `(progn
         (defvar ,name)
         (let* ((sheep (clone* ,sheeple ,properties ,@options)))
           (unless (sheep-nickname sheep)
             (setf (sheep-nickname sheep) ',name))
           (setf ,name sheep)))))

(defun replace-or-reinitialize-sheep (maybe-sheep parents properties &rest options)
  (if (sheep-p maybe-sheep)
      (apply #'reinitialize-sheep maybe-sheep :new-parents parents :new-properties properties options)
      (apply #'spawn-sheep parents properties options)))