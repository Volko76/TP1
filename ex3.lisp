(defun my-assoc (cle a-liste)
  (assoc cle a-liste))

(defun cles (a-liste)
  (mapcar 'car a-liste))

(defun creation (listeCles listeValeurs)
  (mapcar 'cons listeCles listeValeurs))


;(my-assoc 'Pierre '((Yolande 25) (Pierre 22) (Julie 45)))
; Résultat: (Pierre 22)

(my-assoc 'Yves '((Yolande 25) (Pierre 22) (Julie 45)))
; Résultat: NIL

(cles '((Yolande 25) (Pierre 22) (Julie 45)))
; Résultat: (Yolande Pierre Julie)

(creation '(Yolande Pierre Julie) '(25 22 45))
; Résultat: ((Yolande 25) (Pierre 22) (Julie 45))
