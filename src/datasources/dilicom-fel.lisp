(defpackage "BOOKSHOPS.DATASOURCES.DILICOM-FEL"
  (:use "CL")
  (:export "PARSE-LINE")
  (:import-from "LOCAL-TIME" "ENCODE-TIMESTAMP" "+UTC-ZONE+")
  (:import-from "ALEXANDRIA" "MAKE-KEYWORD"))

(in-package  "BOOKSHOPS.DATASOURCES.DILICOM-FEL")

(defstruct (field-info (:constructor make-field-info (name start end &optional (parser #'identity))))
  name 
  start
  end
  parser)

(defun parse-date (string)
  (let ((année (parse-integer string :start 0 :end 4))
        (mois (parse-integer string :start 4 :end 6))
        (jour (parse-integer string :start 6 :end 8)))
    (encode-timestamp 0 0 0 0 jour mois année :timezone +utc-zone+)))

(defun right-trim (string)
  (string-right-trim '(#\Space) string))

(defparameter *basic-fields*
  (list
   (make-field-info :code 0 1
                    (lambda (x) (ecase (make-keyword x)
                                  (:c :création)
                                  (:m :modification-compléte)
                                  (:s :suppression)
                                  (:e :extraction))))
  (make-field-info :ean-13 1 14 #'identity)
  (make-field-info :gencod 14 27 #'identity)
  (make-field-info :date-application 27 35 #'parse-date)
  (make-field-info :disponibilité 35 36
                   (lambda (x)
                     (case (character x)
                       (#\1 :disponible)
                       (#\2 :pas-encore-paru)
                       (#\3 :réimpression-en-cours)
                       (#\4 :non-disponible-provisoirement)
                       (#\5 :ne-sera-plus-distribué-par-nous)
                       (#\6 :Arrêt-définitif-de-commercialisation )
                       (#\7 :manque-sans-date)
                       (#\8 :À-reparaître )
                       (#\9 :abandon-de-parution)
                       (t nil))))
  ;; This is an enum see (p. 18)
  (make-field-info :type-prix 36 37 #'parse-integer)
                        
  (make-field-info :prix-ttc 37 45 #'parse-integer)
  (make-field-info :livre-scolaire 45 47
                   (lambda (x)
                     (ecase (char x 0)
                       (#\1 t)
                       (#\0 nil))))
  (make-field-info :taux-tva1 47 51 #'parse-integer)
  (make-field-info :montant-ht1 51 59 #'parse-integer)
  (make-field-info :taux-tva2 59 63 #'parse-integer)
  (make-field-info :montant-ht2 63 71 #'parse-integer)
  (make-field-info :taux-tva3 71 75 #'parse-integer)
  (make-field-info :montant-ht3 75 83 #'parse-integer)
  ;; This is an enum (p. 22)
  (make-field-info :retour 83 84 #'parse-integer)
  (make-field-info :commandable 84 85
                   (lambda (x)
                     (ecase (character x)
                       (#\1 t)
                       (#\0 nil))))
                        
  ;; I *think* this will always be either #\I or blank, but not sure
  (make-field-info :impression-demande 85 86 #'character)
  ;; 1 = Tout public
  ;; 2 = Réservé aux enseignants 
  (make-field-info :public 86 88 #'parse-integer)
  ;; This is also an enum (p.16)
  (make-field-info :motif-suppression 88 90 #'parse-integer)
  ;; Reserved 90 94
  (make-field-info :date-parution 94 102 #'parse-date)
  ;; Also an enum (p. 19)
  (make-field-info :type-produit 102 104 #'parse-integer)
  (make-field-info :date-fin-commercialisation 104 112 #'parse-date)
  (make-field-info :libellé-standard 112 142 #'right-trim)
  (make-field-info :libellé-caisse  142 162 #'right-trim)
  ;; Also an enum (p. 18)
  (make-field-info :présentation-magasin 162 164 #'parse-integer)
  (make-field-info :epaisseur 164 168 #'parse-integer)
  (make-field-info :largeur 168 172 #'parse-integer)
  (make-field-info :hauteur 172 176 #'parse-integer)
  (make-field-info :poids 176 183 #'parse-integer)
  (make-field-info :libellé-étendu 183 283 #'right-trim)
  (make-field-info :editeur 283 298 #'right-trim)
  (make-field-info :collection 298 313 #'right-trim)
  (make-field-info :auteur 313 333 #'right-trim)
  (make-field-info :présentation-éditeur 333 335 #'right-trim)
  (make-field-info :isbn 335 345 #'identity)
  (make-field-info :référence-fournisseur 345 357 #'right-trim)
  (make-field-info :collection-sérielle 357 367 #'right-trim)
  (make-field-info :théme 367 371 #'parse-integer)
  (make-field-info :isbn-éditeur 371 379 #'right-trim)
  ;; Table :  
  ;; 2 = Annule et remplace le produit lié (B remplace A)
  ;; 4 = Est remplacé par le produit lié (A est remplacé par B) 
  (make-field-info :lien-entre-les-codes 379 380 #'parse-integer)
  (make-field-info :ean-produit-lié 380 393 #'identity)
  (make-field-info :peut-être-commandé 393 394
                   (lambda (x)
                     (ecase (character x)
                       (#\0 nil)
                       (#\1 t))))
  (make-field-info :type-lot 394 395 
                   (lambda (x)
                     (ecase (character x)
                       (#\0 nil)
                       (#\1 t))))
  ;; yet another enumerated field. (p. 22)
  (make-field-info :symbolisation 395 396 #'parse-integer)
  (make-field-info :produit-périssable 396 397 #'parse-integer)
  (make-field-info :nombre-références 397 401 #'parse-integer)
  ))

  
(defun parse-line (line)
  ;; TODO don't use assert
  (unless (member (length line) '(402 485))
    (error 'parse-error))
  (when (> (length line) 402)
    (warn "DILICOM Checkout fields not supported"))
  (loop
    with result = (make-hash-table)
    for field in *basic-fields*
    do
    (with-slots (name start end parser) field
                                        ;(print name) (terpri)
      (let ((raw-value (subseq line start end)))
        (unless (every (lambda (X) (eql x #\Space)) raw-value)
          (setf (gethash name result)
                (funcall parser raw-value)))))
    finally (return result)))
  
(defun parse-file (path)
  (with-open-file (f path)
    (loop for line = (read-line f nil nil)
          while line
          collect (parse-line line))))
