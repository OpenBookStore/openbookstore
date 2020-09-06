(defpackage #:bookshops.datasources.dilicom-flat-text
  (:use #:cl)
  (:export #:parse-line #:parse-file)
  (:import-from #:local-time #:encode-timestamp #:+utc-zone+)
  (:import-from #:alexandria #:make-keyword #:ensure-list)
  (:import-from #:str #:trim-right)
  (:import-from #:split-sequence #:split-sequence))

(in-package  #:bookshops.datasources.dilicom-flat-text)

(defparameter +source-name+ "dilicom-flat-text")

(defstruct (field-info (:constructor make-field-info (name start end &optional (parser #'identity) (representation nil))))
  "Contains information for Dilicom FEL plain-text fields.
start/end are zero-indexed, end-exclusive because that's how indexing
in lisp (and most languages) works, while the specification is
1-indexed, end-inclusive because that's common vernacular (in both
French and English).

In short, start should be 1 less than the specification, end should be
the same, and the start of the following field should be the same as
the end of the previous field"
  name
  start
  end
  parser
  representation)

(defun parse-date (string)
  "Parses date in Dilicom FEL plain-text format: YYYYMMDD"
  (let ((année (parse-integer string :start 0 :end 4))
        (mois (parse-integer string :start 4 :end 6))
        (jour (parse-integer string :start 6 :end 8)))
    (encode-timestamp 0 0 0 0 jour mois année :timezone +utc-zone+)))

(defun format-by-100 (stream value)
  (format stream "~D.~2,'0D" (floor value 100) (mod value 100)))


(defparameter *basic-fields*
  (list
   (make-field-info :code 0 1
                    #'identity
                    (lambda (x) (ecase (make-keyword x)
                                  (:c :created)
                                  (:m :modified)
                                  (:s :deleted)
                                  (:e :extracted))))
   (make-field-info :ean-13 1 14 #'identity)
   (make-field-info :gencod 14 27 #'identity)
   (make-field-info :date-publication 27 35 #'parse-date)
   (make-field-info :availability 35 36 #'parse-integer
                    (lambda (x)
                      (case x
                        (1 :available)
                        (2 :not-yet-published)
                        (3 :reprint)
                        (4 :temporarily-unavailable)
                        (5 :no-longer-available)
                        (6 :permanently-off-market )
                        (7 :indefinitely-out-of-print)
                        (8 :to-reappear )
                        (9 :removed-from-print)
                        (t nil))))
   ;; This is an enum see (p. 18 of FPL)
   (make-field-info :price-type 36 37 #'parse-integer)
   (make-field-info :price 37 45 #'parse-integer
                    (lambda (x)
                      (format nil "~A ~C"
                              (format-by-100 nil (round x 10)) #\Euro_Sign)))
   ;; "livre scolaire" is a specific legal term related to libraries
   (make-field-info :school-book 45 47 #'identity
                    (lambda (x)
                      (ecase (char x 0)
                        (#\1 t)
                        (#\0 nil))))
   (make-field-info :tva-rate-1 47 51 #'parse-integer)
   (make-field-info :before-tax-amount-1 51 59 #'parse-integer)
   (make-field-info :tva-rate-2 59 63 #'parse-integer)
   (make-field-info :before-tax-amount-2 63 71 #'parse-integer)
   (make-field-info :tva-rate-3 71 75 #'parse-integer)
   (make-field-info :before-tax-amount-3 75 83 #'parse-integer)
   ;; This is an enum (p. 22 of FPL)
   (make-field-info :return-code 83 84 #'parse-integer)
   (make-field-info :commandable 84 85 #'identity
                    (lambda (x)
                      (ecase (character x)
                        (#\1 t)
                        (#\0 nil))))
   ;; I *think* this will always be either #\I or blank, but not sure
   (make-field-info :impression-demande 85 86 #'character)
   ;; 1 = Tout public
   ;; 2 = Réservé aux enseignants
   (make-field-info :public 86 88 #'parse-integer
                    (lambda (x)
                      (ecase x
                        (1 :public)
                        (2 :reserved))))
   ;; This is also an enum (p.16 FPL)
   (make-field-info :removal-reason 88 90 #'parse-integer)
   ;; Reserved 90 94
   (make-field-info :publication-date 94 102 #'parse-date)
   ;; Also an enum (p. 19)
   (make-field-info :product-type 102 104 #'parse-integer)
   (make-field-info :end-of-marketing 104 112 #'parse-date)
   (make-field-info :standard-description 112 142 #'trim-right)
   (make-field-info :checkout-description  142 162 #'trim-right)
   ;; Also an enum (p. 18)
   (make-field-info :présentation-magasin 162 164 #'parse-integer)
   (make-field-info :thickness 164 168 #'parse-integer
                    (lambda (x) (format nil "~Dmm" x)))
   (make-field-info :width 168 172 #'parse-integer
                    (lambda (x) (format nil "~Dmm" x)))
   (make-field-info :height 172 176 #'parse-integer
                    (lambda (x) (format nil "~Dmm" x)))
   (make-field-info :weight 176 183 #'parse-integer
                    (lambda (x) (format nil "~Dg" x)))
   (make-field-info :libellé-étendu 183 283 #'trim-right)
   (make-field-info :publisher 283 298 #'trim-right)
   (make-field-info :collection 298 313 #'trim-right)
   ;; per FPL p 5, multiple authors are split with a "/" character
   (make-field-info :authors 313 333 #'trim-right
                    (lambda (x)
                      (split-sequence #\/ x)))
   (make-field-info :présentation-éditeur 333 335 #'trim-right)
   (make-field-info :isbn 335 345 #'identity)
   (make-field-info :référence-fournisseur 345 357 #'trim-right)
   (make-field-info :collection-sérielle 357 367 #'trim-right)
   (make-field-info :théme 367 371 #'parse-integer)
   (make-field-info :isbn-éditeur 371 379 #'trim-right)
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
   (make-field-info :nombre-références 397 401 #'parse-integer))
  "A list of all the field-info structures for the dilicom basic fields")

(defun parse-line (line)
  ;; per FEL Plain-text, lines are either 401 or 484
  ;; implied is a #\Return at the end-of-the line
  ;; making 402 or 485
  (unless (member (length line) '(402 485))
    (error 'parse-error))
  (when (> (length line) 402)
    (warn "DILICOM Checkout fields not supported"))
  (loop
    with result = (make-hash-table)
    for field in *basic-fields*
    do
    (with-slots (name start end parser representation) field
                                        ;(print name) (terpri)
      (let ((raw-value (subseq line start end)))
        (unless (every (lambda (X) (eql x #\Space)) raw-value)
          (if (null representation)
              (setf (gethash name result)
                    (funcall parser raw-value))
              (let* ((parsed-value (funcall parser raw-value))
                     (repr-value (funcall representation parsed-value)))
                (setf
                 (gethash (make-keyword (format nil "~A-REPR" name)) result) repr-value
                 (gethash name result) repr-value
                 (gethash (make-keyword (format nil "~A-RAW" name)) result) parsed-value))))))
    finally (return result)))

(defun ensure-external-fields (parsed-line)
  "This ensures certain fields will exist in the output to match the
fields in the old dilicomScaper.py
"
  (setf
        (gethash :publishers parsed-line) (ensure-list (gethash :publisher parsed-line))
        (gethash :price-currency parsed-line) #\Euro_Sign
        (gethash :price-divisor parsed-line) 1000
        (gethash :publishers-repr parsed-line) (gethash :publisher parsed-line)
        (gethash :source-name parsed-line) (copy-seq +source-name+)
        ;; no img
        ;; no summary
        ;; no search-terms
        )
  parsed-line)

(defun parse-file (path)
  (with-open-file (f path)
    (loop for line = (read-line f nil nil)
          while line
          collect (ensure-external-fields (parse-line line)))))
