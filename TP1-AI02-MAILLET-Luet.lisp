;===========EXERCICE 1=============
(defun reverseA (a b c)
  (write (list c b a)))
    ;On range à la main les 3 éléments
    ;On met tout sous forme de liste et on l'affiche avec write
(defun reverseB (L)
        ;(cadr L) = car(cdr L)
        (if (= 3 (list-length L)) ; Si la liste contient 3 éléments
            (return-from reverseB (list (caddr L) (cadr L) (car L))))
            ;On prend le premier élement du reste du reste de L avec "(cadr (cdr L))" et on le met en 1ème position
            ;On prend le premier élement du reste de L avec "(cadr L)" et on le met en 2ème position
            ;On prend le premier élement de L avec "(car L)" et on le met en 3ème position
        (if (= 2 (list-length L)) ;2 éléments
            (return-from reverseB (list (cadr L) (car L))))
            ;Idem mais on met que le premier et deuxième élement
        (if (= 1 (list-length L)) ;1 élément
            (return-from reverseB (list (car L))))
            ;Idem mais a final on renvoit juste l'unique élément
        ;On met tout sous forme de liste et on l'affiche avec write)
)

(defun reverseC (L)
  (let ((newList '())) ; On créer une nouvelle liste
    (dolist (element L newList) ;On parcours tous les élements de L
      (setq newList (cons element newList)))))  ; On définit la nouvelle liste à la nouvelle liste auquel on concatene l'element.
      ;à la fin du let on renvoie la fonction
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

(defun monReverse (L)
  (let ((newList '())) ; On créer une nouvelle liste
    (dolist (element L newList) ;On parcours tous les élements de L
      (if (atom element)  ;On regarde si c'est un element
        (setq newList (cons element newList))  ;Si oui on définit la nouvelle liste à la nouvelle liste auquel on concatene l'element.
        (setq newList (cons (monReverse element) newList)) ;Sinon on fait la meme mais avec un appel récursif pour inverser la sous liste
        )
    )
  )    ;à la fin du let on renvoie la fonction
)
(defun palindrome (L)
  (equal L (monReverse L)))

;(reverseA 1 2 3)
;(write (reverseB '(1)))
;(write (reverseC '( a b (c d) e f)))
;(double '((1 2) 3 (4 5) 6))
;(nombres3 '(1 A 3 A B C))
;(grouper '(1 2 3) '(4 5 6))
;(write (monReverse '( a b (c d) e f)))
;(write (palindrome '(x a m a x)) )
;===========EXERCICE 2=============
(defun list-triple-couple (lst)
  (mapcar (lambda (x) (list x (* x 3))) lst))

;===========EXERCICE 3=============

(defun my-assoc (cle a-liste)
  (assoc cle a-liste))

(defun cles (a-liste)
  (mapcar 'car a-liste))

(defun creation (listeCles listeValeurs)
  (mapcar 'cons listeCles listeValeurs))


;(my-assoc 'Pierre '((Yolande 25) (Pierre 22) (Julie 45)))
; Résultat: (Pierre 22)

;(my-assoc 'Yves '((Yolande 25) (Pierre 22) (Julie 45)))
; Résultat: NIL

;(cles '((Yolande 25) (Pierre 22) (Julie 45)))
; Résultat: (Yolande Pierre Julie)

;(creation '(Yolande Pierre Julie) '(25 22 45))
; Résultat: ((Yolande 25) (Pierre 22) (Julie 45))

;===========EXERCICE 4=============

(write "Hello")
;=========== Question 1 (3 points)=============
;Écrire les différentes fonctions d'accès aux différentes composantes d'une tombe :
;nom, an_inhum, num, rangee, debut_loc, durée_loc.
;Création des structures
(defstruct cimetiere
    nom
    tombe_list)
(defstruct tombes
    nom
    an_inhum
    num
    rangee
    debut_loc
    durée_loc)
;Création des fonctions d'instanciation
(defun make_cimetiere(nom tombe_list)
    (make-instance 'cimetiere
        :nom nom
        :tombe_list tombe_list))
(defun make_tombes(nom an_inhum num rangee debut_loc durée_loc)
    (make-instance 'tombes
        :nom nom
        :an_inhum an_inhum
        :num num
        :rangee rangee
        :debut_loc debut_loc
        :durée_loc durée_loc))
;Création des fonctions d'accès
;Cimetiere
(defun cimetiere_nom (cimetiere)
  (slot-value cimetiere 'nom))
(defun cimetiere_tombes (cimetiere)
  (slot-value cimetiere 'tombe_list))
;Tombe
(defun tombe_nom (tombes)
  (slot-value tombes 'nom))
(defun tombe_an_inhum (tombes)
  (slot-value tombes 'an_inhum))
(defun tombe_num (tombes)
  (slot-value tombes 'num))
(defun tombe_rangee (tombes)
  (slot-value tombes 'rangee))
(defun tombe_debut_loc (tombes)
  (slot-value tombes 'debut_loc))
(defun tombe_duree_loc (tombes)
  (slot-value tombes 'durée_loc))
;=========== Question 2 (1 point) =============
;Écrire une fonction qui-est-là qui, étant donné un emplacement (numéro + rangée) et un cimetière,
;retourne le nom de la personne qui y est enterrée.
(defun qui_est_la(cimetiere num rangee)
  (dolist (tombe (cimetiere_tombes cimetiere))  ;On parcourt toutes les tombes
    (if (and (= (tombe_num tombe) num) (= (tombe_rangee tombe) rangee)) ;Si la tombe courante a la meme rangée et le meme numéro que celui qu'on cherche
      (return-from qui_est_la (tombe_nom tombe))    ;On s'arrete la et on renvoie le résulat
    )
  )
(write "Emplacement non attribué"))
;=========== Question 3 (1 point) =============
;Spécifier puis écrire un prédicat prévoyant? qui, étant donné une tombe, retourne vrai si son occupant
;a acheté sa concession par anticipation (avant son décès), faux sinon. Pour cette question ne pas
;utiliser de structure de contrôle conditionnelle.
(defun predicat_prevoyant(tombe)
  (< (tombe_debut_loc tombe) (tombe_an_inhum tombe))) ;Si le début de la loc est plus vielle que l'inhumation (donc que sa date est plus petite)
;=========== Question 4 (1 point) =============
;Écrire une fonction nb-prévoyants qui, étant donné un cimetière, compte le nombre de résidents ayant
;acheté leur concession par anticipation
(defun nb_prevoyants(cimetiere)
  (let ((count 0))    ;On créer une variable locale count qu'on initialise à 0
    (dolist (tombe (cimetiere_tombes cimetiere))  ;Pour chaque tombe
        (when (predicat_prevoyant tombe)  ;On check si la tombe est prévoyante
          (incf count)) ;Si oui on incrémente le compteur
      )
    count ;Une fois qu'on a check toutes les tombes, le let renvoie count
  ) ;Puis la fonction renvoie le resulat du let (count)
)

;=========== Question 5 (2 points) =============
;Écrire une fonction annuaire qui, étant donné un cimetière et un entier naturel n, retourne la liste des
;noms des résidents de la rangée n
(defun annuaire(cimetiere n)
  (let ((result '()))   ;On créer une liste locale qui va contenir toutes les tombes de la rangée
    (dolist (tombe (cimetiere_tombes cimetiere))  ;On parcourt toutes les tombes
      (if (= (tombe_rangee tombe) n) ;Si la tombe courante a la meme rangée que celui qu'on cherche
        (push (tombe_nom tombe) result) ;On la rajoute à la liste des tombes de la rangée
      )
    )
  result) ;On renvoie le résultat
)
;=========== Question 6 (2 points) =============
;Écrire une fonction doyen-benjamin qui, étant donné un cimetière, retourne le résident le plus ancien
;et le résident le plus récent. On s’attachera à proposer une solution à la fois la plus élégante et la plus
;efficace possible.
(defun doyen_benjamin(cimetiere)
  (setq oldest (car (cimetiere_tombes mon_cimetiereB))) ;On prend la premiere tombe comme reference pour éviter de comparer à un nil apres
  (setq youngest oldest)  ;Idem pour youngest
  (dolist (tombe (cimetiere_tombes cimetiere))  ;On parcourt toutes les tombes
    (if (< (tombe_an_inhum tombe) (tombe_an_inhum oldest)) ;Si la tombe courante est plus vielle que oldest (que son année d'inhumation est plus petite)
      (setq oldest tombe) ;On la définit comme la tombe la plus vielle pour l'instant
    )
    (if (> (tombe_an_inhum tombe) (tombe_an_inhum youngest)) ;Si la tombe courante est plus jeune que youngest (que son année d'inhumation est plus grande)
      (setq youngest tombe);On la définit comme la tombe la plus jeune pour l'instant
    )

  )(list oldest youngest)   ;On renvoie la plus vielle et la plus récente
)
;============================================
; Tests

(setq tombe1 (make-tombes :nom "Bécoud" :an_inhum 2001 :rangee 45 :num 17 :debut_loc 2000 :durée_loc 30))
(setq tombe2 (make-tombes :nom "Grappelli" :an_inhum 1997 :rangee 85 :num 23 :debut_loc 1997 :durée_loc 5))
(setq tombe3 (make-tombes :nom "Desproges" :an_inhum 1988 :rangee 11 :num 6 :debut_loc 1988 :durée_loc 30))
(setq tombe4 (make-tombes :nom "Morrison" :an_inhum 1971 :rangee 6 :num 12 :debut_loc 1971 :durée_loc 30))
(setq tombe5 (make-tombes :nom "Mouloudji" :an_inhum 1994 :rangee 6 :num 9 :debut_loc 1990 :durée_loc 15))
(setq tombe6 (make-tombes :nom "Nohain" :an_inhum 1981 :rangee 89 :num 9 :debut_loc 1979 :durée_loc 15))
(setq mon_cimetiereB (make-cimetiere :nom "CimetiereB" :tombe_list (list tombe1 tombe2 tombe3 tombe4 tombe5 tombe6)))
;(write (cimetiere_nom mon_cimetiereB))
;(write (tombe_nom (nth 1 (cimetiere_tombes mon_cimetiereB))))
;(print "\n")
;(write (qui_est_la mon_cimetiereB 23 85))
;(write (predicat_prevoyant tombe1))
;(print "\n Nombre de prévoyants :")
;(write (nb_prevoyants mon_cimetiereB))
;(print "\n")
;(write (annuaire mon_cimetiereB 6))
;(write (doyen_benjamin mon_cimetiereB))
