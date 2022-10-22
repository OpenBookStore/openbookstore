(in-package :bookshops/web)

#+nil
(log:config :debug)

(defroute api-card-add-one ("/api/card/add" :method :post)
    (&post (id :parameter-type 'integer))
  (log:debug "Requested id: ~a.~&" id)
  (let ((card (models:find-by :id id)))
    (assert card)
    (princ-to-string
     (models:add-to (models:default-place) card))))

(defroute api-card-remove-one ("/api/card/remove" :method :post)
    (&post (id :parameter-type 'integer))
  (let ((card (models:find-by :id id)))
    (assert card)
    (princ-to-string
     (models:add-to (models:default-place)
                              card
                              :quantity -1))))

(defroute api-receive-card ("/api/receive" :method :post)
    (&post (counter :parameter-type 'integer)
           (shelf-id :parameter-type 'integer :real-name "shelf_id")
           q)
  "Receive a query (ISBN so far), look for it and return the full data.
  The counter is used on the client side to update the corresponding node."
  ;;Changes:
  ;; - Should add a card for new ISBNs automatically
  ;; - Check-in-stock is not done by search anymore. Done here.
  ;; - Adding keyword search should be easy: just remove :remote-key and
  ;;   :local-key parameters below.
  (setf (hunchentoot:content-type*) "application/json")
  ;; (sleep 2)                             ;debug

  (log:debug shelf-id)
  (cl-json:encode-json-plist-to-string
   (print (if (and q
                   (not (str:blankp q))
                   (<= 3 (length q)))
              (list :counter counter
                    :card
                    (first                   ; get-or-search-single = 1 result please.
                     (models::check-in-stock ; gives back a list :/
                      (get-or-search-single q :remote-key nil :local-key nil
                                            :save t )
                      :shelf-id shelf-id)))
              (list :counter counter
                    :card "bad query")))))

(models:define-role-access route-card-edit :view :editor)
(defroute route-card-edit ("/api/card/update" :method :POST
                                              :decorators ((easy-routes:@json)))
    (&post (card-id :real-name "cardId" :parameter-type 'integer)
           (shelf-id :real-name "shelfId" :parameter-type 'integer))
  "Update a Card.
  Currently only the shelf: called from card.js with the shelf select.
  The complete card update form is in card-update/post-route.
  Changing the shelf works when we choose the blank option from the HTML select too:
  FIND-SHELF-BY :id NIL returns NIL, and it works. Handy."
  (log:info "updating card: " card-id shelf-id)
  (let ((card (models:find-by :id card-id))
        (shelf (models::find-shelf-by :id shelf-id)))
    (log:info "existing card: " card)
    (setf (models::shelf card) shelf)
    (log:info "new card: " card)
    (mito:save-dao card)

    (cl-json:encode-json-to-string (dict "status" 200))))

(models:define-role-access api-quick-search :view :editor)
(defroute api-quick-search
    ("/api/quick-search" :decorators ((@check-roles stock-route) (easy-routes:@json))) (&get q)
  (cl-json:encode-json-plist-to-string (quick-search q)))

;;(models:define-role-access api-sell-search :view :editor)
(defroute api-sell-search
    ("/api/sell-search" :decorators ((@check-roles stock-route) (easy-routes:@json))) (&get q)
  (cl-json:encode-json-plist-to-string (sell-search q)))

(defun split-post-var (pvar)
  (str:split #\] (substitute #\] #\[ pvar) :omit-nulls t))

(defun extract-array (name params)
  "Elements of an array of objects sent in a POST request arrive with keys like: 'arrayname[0][key]'. This function joins all of the elements into a list of alists."
  (let* ((params (remove-if-not (lambda (itm) (str:starts-with? name (car itm))) params))
         (params (sort params #'string< :key #'car))
         (last nil)
         (accum nil)
         (res nil))
    (dolist (p params)
      (destructuring-bind (_ index key) (split-post-var (car p))
        (declare (ignore _))
        (unless (string-equal index last)
          (when last
            (push (nreverse accum) res)
            (setf accum nil))
          (setf last index))
        (push (cons (alexandria:make-keyword (string-upcase key)) (cdr p)) accum)))
    (when accum (push (nreverse accum) res))
    (nreverse res)))

(defvar *sell-data* nil)

(defun sell-complete (&key books client sell-date
                        payment-method-id payment-method-name)
  (models:make-sale
   :payment-method-id (parse-number payment-method-id)
   :payment-method-name payment-method-name
   :client client
   :date sell-date
   :books
   (mapcar (lambda (book)
             (list (cons :id (parse-number (access book :id)))
                   (cons :quantity (parse-number (access book :quantity)))
                   (cons :price (if (str:empty? (access book :price))
                                    ""
                                    (parse-number:parse-number (access book :price))))))
           books))
  (list :success t))

(models:define-role-access api-sell-complete :view :editor)
(defroute api-sell-complete
    ("/api/sell-complete/" :method :post :decorators ((@check-roles stock-route) (easy-routes:@json)))
    (&post (payment-method-id :real-name "paymentMethodId")
           (payment-method-name :real-name "paymentMethodName")
           client
           (sell-date :real-name "sellDate"))

  (let ((params
          (list :books (extract-array "books" (hunchentoot:post-parameters*))
                :payment-method-id payment-method-id
                :payment-method-name payment-method-name
                :client client
                :sell-date (utils:parse-iso-date sell-date))))
    (setf *sell-data* params)
    (cl-json:encode-json-plist-to-string
     (apply #'sell-complete params))))
