(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(defun main ()
  (format t "Enter a number: ")
  (let ((input (read)))
    (format t "The factorial of ~d is ~d.~%" input (factorial input))))

(main)