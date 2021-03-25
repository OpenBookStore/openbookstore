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
                     (get-or-search-single isbn :remote-key nil :local-key nil
                                         :save t )))
              (list :counter counter
                    :card "no isbn")))))

(bookshops.models:define-role-access api-quick-search :view :editor)
(defroute api-quick-search
    ("/api/quick-search" :decorators ((@check-roles stock-route) (easy-routes:@json))) (&get q)
  (cl-json:encode-json-plist-to-string (quick-search q)))

;;(bookshops.models:define-role-access api-sell-search :view :editor)
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

(defun sell-complete (&key books payment-method client sell-date)
  (models:make-sale
   :payment payment-method
   :client client
   :date sell-date
   :books
   (mapcar (lambda (book)
             (list (cons :id (parse-number:parse-number (access book :id)))
                   (cons :quantity (parse-number:parse-number (access book :quantity)))
                   (cons :price (if (str:empty? (access book :price))
                                    ""
                                    (parse-number:parse-number (access book :price))))))
           books))
  (list :success t))

(bookshops.models:define-role-access api-sell-complete :view :editor)
(defroute api-sell-complete
    ("/api/sell-complete/" :method :post :decorators ((@check-roles stock-route) (easy-routes:@json)))
    (&post (payment-method :real-name "paymentMethod")
           client
           (sell-date :real-name "sellDate"))
  (let ((params
          (list :books (extract-array "books" (hunchentoot:post-parameters*))
                :payment-method payment-method
                :client client
                :sell-date (utils:parse-iso-date sell-date))))
    (setf *sell-data* params)
    (cl-json:encode-json-plist-to-string
     (apply #'sell-complete params))))
