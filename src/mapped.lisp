(in-package :cl-user)

(defstruct (map (:predicate mapp)
                (:constructor %make-map))
  parents properties roles hierarchy-list child-maps)

(defstruct (object (:predicate objectp)
                   (:constructor %make-object))
  map property-values)

(defvar *maps* (make-hash-table :test 'equal))

(defun tree-find-if (test tree &key (key #'identity))
  (cond ((null tree) nil)
        ((atom tree)
         (when (funcall test (funcall key tree))
           tree))
        (t (or (tree-find-if test (car tree) :key key)
               (tree-find-if test (cdr tree) :key key)))))

(defun find-map (parents properties)
  (tree-find-if (lambda (map) (equal properties (map-properties map)))
                (gethash parents *maps*)))

(defun make-object (parents properties)
  (let ((maybe-map (find-map parents properties)))
    (if (and maybe-map 
             (every #'eq properties (map-properties maybe-map)))
        (%make-object :map maybe-map
                      :property-values (make-array (length (map-properties map))))
        (%make-object :map (make-map :parents parents 
                                     :properties properties)
                      :property-values (make-array (length properties))))))
