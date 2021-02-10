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
  The counter is used on the client side to update the corresponding node.
  TODO: actually add in stock :)"
  (setf (hunchentoot:content-type*) "application/json")
  ;; (sleep 1)
  (cl-json:encode-json-plist-to-string
   (print (if (and isbn (not (str:blankp isbn)))
              (list :counter counter
                    :card (get-or-search isbn))
              (list :counter counter
                    :card "no isbn")))))
