(defun list-triple-couple (lst)
  (mapcar (lambda (x) (list x (* x 3))) lst))
