;(defun Transfo (L) (list (cadr L)(car L)(caddr L)))
;(write(Transfo ((n + 2) / (n - 3))))


; ((n + 2) / (n - 3))      ->    (/ (+ n 2) (- n 3))


(defun reverseA (a b c)
  (write (list c b a))) 
    ;On range à la main les 3 éléments
    ;On met tout sous forme de liste et on l'affiche avec write
(defun reverseB (L)
    ;(cadr L) = car(cdr L)
    (if (= 3 (list-length L)) ; Si la liste contient 3 éléments
        (write (list (cadr (cdr L)) (cadr L) (car L))))
        ;On prend le premier élement du reste du reste de L avec "(cadr (cdr L))" et on le met en 1ème position
        ;On prend le premier élement du reste de L avec "(cadr L)" et on le met en 2ème position
        ;On prend le premier élement de L avec "(car L)" et on le met en 3ème position
    (if (= 2 (list-length L)) ;2 éléments
        (write (list (cadr L) (car L))))
        ;Idem mais on met que le premier et deuxième élement 
    (if (= 1 (list-length L)) ;1 élément
        (write (list (car L)))))
        ;Idem mais a final on renvoit juste l'unique élément
    ;On met tout sous forme de liste et on l'affiche avec write
    
    
(defun reverseC (L)
    (loop for i from 0 to (- (list-length L) 1) do  ;On parcourt la liste de 0 à len(L)-1
        (write (nth (- (list-length L) (+ i 1)) L))))    
(defun reverseCBis (L)
    (write (reverse L)))    ;Il y a une fonction préintégrée pour inverser
(defun double (L)
    (loop for i from 0 to (- (list-length L) 1) do  ;Pour chaque élément
        (if (atom (nth i L))                        ;Si c'est un atome
            (write (nth i L)))                      ;On l'affiche une deuxième fois
        (write (nth i L))))                         ;Peut importe ce que c'est on l'affiche au moins une fois
(defun nombres3 (L)
    (if (and (numberp (nth 0 L)) (numberp (nth 1 L)) (numberp (nth 2 L))) ; Si le premier, deuxième et troisieme element sont des nombres (numberp renvoie T si l'atom est un nombre)
        (write "GOOD")              ;Si oui Ecrire GOOD
        (write "PERDU")))           ;Sinon ecrire PERDU

(defun grouper (L1 L2)
    (setq newList (append L1 L2))       ;On regroupe les deux listes
    (setq newList2 ())                  ;On créer une liste vide qui va se remplir avec la liste modifiée mais inversée
    (loop for i from 0 to (- (list-length newList) 1) by 2 do   ;Boucle for avec une incrémentation de 2
        (push (list (nth i newList) (nth (+ i 1) newList)) newList2))   ;On ajoute à newListe2 une miniliste composée de l'élément d'indice i et i+1
    (setq newList (reverse newList2))       ;La liste étant inversée, on la remet dans le bon sens et on réutilise pour cela la liste newList pour des raisons de lisibilité et d'optimisation de mémoire
    (write newList))    ;On l'écrit dans la console pour débugguer


(defun palindrome (L)
  (equal L (reverseC L)))

;(reverseA 1 2 3)
;(reverseB '(1))
;(reverseC '( a b (c d) e f))
;(double '((1 2) 3 (4 5) 6))
;(nombres3 '(1 A 3 A B C))
;(grouper '(1 2 3) '(4 5 6))
(write (palindrome '(x a m a x)) )
