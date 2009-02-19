(in-package :sheeple)

(defmethod print-object :around (sheep stream)
  (if (sheep-p sheep)
      (print-sheep sheep stream)
      (call-next-method)))