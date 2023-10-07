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
    (setq oldest NIL)
    (setq youngest NIL)
    (dolist (tombe (cimetiere_tombes cimetiere))  ;On parcourt toutes les tombes
      (if (< (tombe_an_inhum tombe) oldest) ;Si la tombe courante est plus vielle que oldest (que son année d'inhumation est plus petite)
        (setq oldest tombe) ;On la définit comme la tombe la plus vielle pour l'instant
      )
      (if (> (tombe_an_inhum tombe) youngest) ;Si la tombe courante est plus jeune que youngest (que son année d'inhumation est plus grande)
        (setq youngest tombe);On la définit comme la tombe la plus jeune pour l'instant
      )
    (oldest youngest)   ;On renvoie la plus vielle et la plus récente
  )
  ;============================================
  ; Tests

  (setq tombe1 (make_tombes "Bécoud" 2001 45 17 2000 30))
  (setq tombe2 (make_tombes "Grappelli" 1997 85 23 1997 5))
  (setq tombe3 (make_tombes "Desproges" 1988 11 6 1988 30))
  (setq tombe4 (make_tombes "Morrison" 1971 6 12 1971 30))
  (setq tombe5 (make_tombes "Mouloudji" 1994 42 9 1990 15))
  (setq tombe6 (make_tombes "Nohain" 1981 89 9 1979 15))
  (setq mon_cimetiereB (make_cimetiere "CimetiereB" (list tombe1 tombe2 tombe3 tombe4 tombe5 tombe6)))
  ;(write (cimetiere_nom mon_cimetiereB))
  ;(write (tombe_nom (nth 1 (cimetiere_tombes mon_cimetiereB))))
  ;(print "\n")
  ;(write (qui_est_la mon_cimetiereB 55 23))
  ;(write (predicat_prevoyant (nth 2 (cimetiere_tombes mon_cimetiereB))))
  ;(print "\n Nombre de prévoyants :")
  ;(write (nb_prevoyants mon_cimetiereB))
  ;(print "\n")
  ;(write (annuaire mon_cimetiereB 9))

