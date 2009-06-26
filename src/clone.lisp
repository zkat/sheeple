;; clone.lisp
;;
;; Contains the clone macro and utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(defun canonize-sheeple (sheeple)
  `(list ,@sheeple))

(defun canonize-properties (properties)
  `(list ,@(mapcar #'canonize-property properties)))

(defun canonize-properties* (properties)
  `(list ,@(mapcar #'canonize-property* properties)))

(defun canonize-property* (property)
  "This version is fancier and adds readers/writers automatically"
  (let ((name (car property))
        (value (cadr property))
        (readers nil)
        (writers nil)
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
        (otherwise 
         (pushnew (cadr olist) other-options)
         (pushnew (car olist) other-options))))
    (unless (or readers no-reader-p)
      (pushnew name readers))
    (unless (or writers no-writer-p)
      (pushnew `(setf ,name) writers))
    `(list ',name ,value
           ,@(when readers `(:readers ',readers))
           ,@(when writers `(:writers ',writers))
           ,@other-options)))

(defun canonize-property (property)
  (let ((name (car property))
        (value (cadr property))
        (readers nil)
        (writers nil)
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
        (otherwise 
         (pushnew (cadr olist) other-options)
         (pushnew (car olist) other-options))))
    `(list ',name ,value
           ,@(when readers `(:readers ',readers))
           ,@(when writers `(:writers ',writers))
           ,@other-options)))

(defun canonize-clone-options (options)
  (mapappend #'canonize-clone-option options))

(defun canonize-clone-option (option)
  (list `',(car option) (cadr option)))

(defmacro defclone (sheeple properties &rest options)
  "Standard sheep-generation macro. This variant auto-generates accessors."
  `(let ((sheep (spawn-sheep
                 ,(canonize-sheeple sheeple)
                 ,@(canonize-clone-options options))))
     (mapcar (lambda (prop-spec)
               (apply #'add-property sheep prop-spec)) ,(canonize-properties* properties))
     sheep))

