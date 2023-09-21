(defun reverseA (arg1 arg2 arg3)
    (format t "~a  ~a ~a" arg3 arg2 arg1))
  
(defun reverseB (&rest args)
    (reverse args))

(defun reverseC (&rest args)
    (reverse args))

(defun double (L)
    (loop for i from 1 to 15 do     ;On fait un boucle qui fait monter la variable i de 1 à 15
        (if (atom (nth i L))        ;On regarde si l'objet à l'index i est un atom
            ()                      ;Si oui
            ())))                   ;Si non