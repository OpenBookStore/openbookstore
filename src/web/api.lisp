(in-package :bookshops-web)

#+nil
(log:config :debug)

(defroute api-card-add-one ("/api/card/add" :method :post)
    (&post (id :parameter-type 'integer))
  (log:debug "Requested id: ~a.~&" id)
  (let ((card (bookshops.models:find-by :id id)))
    (assert card)
    (princ-to-string
     (bookshops.models:add-to (bookshops.models:default-place) card))))

(defroute api-card-remove-one ("/api/card/remove" :method :post)
    (&post (id :parameter-type 'integer))
  (let ((card (bookshops.models:find-by :id id)))
    (assert card)
    (princ-to-string
     (bookshops.models:add-to (bookshops.models:default-place)
                              card
                              :quantity -1))))

(defroute api-receive-card ("/api/receive" :method :post)
    (&post (counter :parameter-type 'integer)
           isbn)
  "Receive an ISBN, look for it and return the full data.
  The counter is used on the client side to update the corresponding node."
  ;;Changes:
  ;; - Should add a card for new ISBNs automatically
  ;; - Check-in-stock is not done by search anymore. Done here.
  ;; - Adding keyword search should be easy: just remove :remote-key and
  ;;   :local-key parameters below.
  (setf (hunchentoot:content-type*) "application/json")
  ;; (sleep 1)
  (cl-json:encode-json-plist-to-string
   (print (if (and isbn (not (str:blankp isbn)))
              (list :counter counter
                    :card
                    (models::check-in-stock
                     (get-or-search isbn :remote-key nil :local-key nil
                                         :save t :multiple nil)))
              (list :counter counter
                    :card "no isbn")))))
