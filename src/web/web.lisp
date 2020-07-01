(in-package :bookshops-web)

#|
Show the stock on a web page.
(in construction)

(connect)
then
(start-app)

and go to localhost:4242/stock/

In this file:
- custom Djula filters
- templates loading
- routes
- server start/stop

Dev helpers:
- adding ?raw=t on a URL (on a card page)
|#

(defvar *server* nil
  "Current instance of easy-acceptor.")

(defparameter *port* 4242)

;;; Djula filters.
(djula:def-filter :price (val)
  (format nil "~,2F" val))

(djula:def-filter :url (card)
  "Create a full URL to uniquely identify this card."
  (format nil "/~a/~a-~a"
          "card"                       ; this can be translated
          (mito:object-id card)
          ;; the slug won't actually be read back, only the id.
          (slug:slugify (title card))))

(djula:def-filter :quantity (card)
  (typecase card
    (bookshops.models:book (quantity card))
    (bookshops.models:place-copies (bookshops.models:place-copy-quantity card))
    (t (or (access:access card :in-stock) 0))))

(djula:def-filter :name (obj)
  (format nil "~a" (name obj)))

(djula:def-filter :describe (card)
  (with-output-to-string (s)
    (describe card s)))

;;; stolen options read-from-string idea from the djula time filter
(djula:def-filter :quantity-style (quantity raw-options)
  (let ((options (read-from-string raw-options)))
    (cond ((= 0 quantity) (access:access options :zero))
          ((plusp quantity) (access:access options :positive))
          (t (access:access options :negative)))))

;;; Load templates.
(djula:add-template-directory
 (asdf:system-relative-pathname "bookshops" "src/web/templates/"))
(defparameter +base.html+ (djula:compile-template* "base.html"))
(defparameter +dashboard.html+ (djula:compile-template* "dashboard.html"))
(defparameter +search.html+ (djula:compile-template* "search.html"))
(defparameter +stock.html+ (djula:compile-template* "stock.html"))
(defparameter +card-page.html+ (djula:compile-template* "card-page.html"))
(defparameter +card-stock.html+ (djula:compile-template* "card-stock.html"))

(defparameter +404.html+ (djula:compile-template* "404.html"))

;;; search
;;; TODO find somewhere better to put search functionatlity this.
(defvar *search-cache* (cacle:make-cache 5000 '%search-datasources :test 'equal
                                         :lifetime (* 24 3600)))

(defun %search-datasources (q)
  (declare (type string q))
  (cond
    ;; ISBN? Dilicom search.
    ((bookshops.utils::isbn-p q)
     (values (dilicom:search-books (list q)) 1))

    ;; Free search? Other datasources.
    ((not (str:blank? q))
     (values (fr:books q) 1))

    (t (values nil 1))))

(defun search-datasources (query)
  (cacle:with-cache-fetch res (*search-cache* query)
    (when res
      (bookshops.models::check-in-stock res))))

;;; Routes.
(defroute home-route ("/") ()
  (djula:render-template* +dashboard.html+ nil
                          :route "/"
                          :current-user (current-user)
                          :data (list :nb-titles (bookshops.models:count-book)
                                      :nb-books (bookshops.models::total-quantities)
                                      :nb-titles-negative (length
                                                           (bookshops.models::negative-quantities)))))

(bookshops.models::define-role-access stock-route :view :stock-owner)
(defroute stock-route ("/stock" :decorators ((@check-roles stock-route)))
    (&get q)
  (let ((cards (cond
                 ((bookshops.models::isbn-p q)
                  (list (find-by :isbn q)))
                 (q
                  (find-book :query (bookshops.utils::asciify q)))
                 (t
                  ;; XXX: pagination
                  (subseq (find-book)
                          0
                          (min 50 (bookshops.models::count-book)))))))
    (djula:render-template* +stock.html+ nil
                            :route "/stock"
                            :current-user (current-user)
                            :cards cards
                            :nb-results (length cards)
                            :q q
                            :data (list :nb-titles (bookshops.models:count-book)
                                        :nb-books (bookshops.models::total-quantities)
                                        :nb-titles-negative (length
                                                             (bookshops.models::negative-quantities))))))

(defroute search-route ("/search") (&get q)
  (let ((cards (and q (search-datasources q))))
    (if cards
        (djula:render-template* +search.html+ nil
                                :route "/search"
                                :q q
                                :cards cards
                                :nb-results (length cards)
                                :title (format nil "OpenBookstore - search: ~a" q))
        (djula:render-template* +search.html+ nil
                                :route "/search"
                                :q q
                                :messages (list "Please enter an ISBN or some keywords.")))))

(defroute add-or-create-route ("/card/add-or-create/" :method :post)
    (q title isbn cover-url publisher (updatep :parameter-type 'boolean
                                               :init-form t)
       (book-id :parameter-type 'string :init-form "")
       (referer-route :parameter-type 'string :init-form "/search"))
  (let* ((book
          (if (str:blank? book-id)
              (find-existing (make-book :title title :isbn isbn :cover-url cover-url
                                        :publisher publisher)
                             :update updatep)
              (find-by :id book-id))))
    (save-book book)
    (djula:render-template* +card-page.html+ nil
                            :q q
                            :card book
                            :referer-route referer-route
                            :places-copies
                            (bookshops.models::book-places-quantities book)
                            :places (bookshops.models:find-places))))

(defun redirect-to-search-result (route query book)
  (hunchentoot:redirect
   (format nil "~a~@[?q=~a~]#card~a" route
           (and (str:non-empty-string-p query) query)
           (bookshops.models::object-id book))))

(defroute card-add-stock-route ("/card/add-stock/" :method :post)
    (q place-id (quantity :parameter-type 'integer :init-form 0) isbn
       (referer-route :parameter-type 'string :init-form "/search"))
  (let ((card (find-by :isbn isbn))
        (place (find-place-by :id place-id)))
    (bookshops.models:add-to place card :quantity quantity)
    (redirect-to-search-result referer-route q card)))

(defroute card-quick-add-route ("/card/quick-add-stock/" :method :post)
    (q (quantity :parameter-type 'integer :init-form 1) title isbn cover-url publisher
       (updatep :parameter-type 'boolean :init-form t)
       (book-id :parameter-type 'string :init-form "")
       (referer-route :parameter-type 'string :init-form "/search"))
  (let ((book
         (if (str:blank? book-id)
             (find-existing
              (make-book :title title :isbn isbn :cover-url cover-url
                         :publisher publisher)
              :update updatep)
             (find-by :id book-id))))
    (save-book book)
    (bookshops.models:add-to (default-place) book :quantity quantity)
    (redirect-to-search-result referer-route q book)))

(defroute card-page ("/card/:slug") (&get raw)
  "Show a card.

  If the URL parameter RAW is \"t\" (the string), then display the card object literally (with describe)."
  (let* ((card-id (ignore-errors
                    (parse-integer (first (str:split "-" slug)))))
         (card (when card-id
                 (mito:find-dao 'book :id card-id))))
    (cond
      ((null card-id)
       (djula:render-template* +404.html+ nil))
      (card
       (djula:render-template* +card-stock.html+ nil
                               :messages nil
                               :route "/stock"
                               :card card
                               :places-copies (bookshops.models::book-places-quantities card)
                               :raw raw))
      (t
       (djula:render-template* +404.html+ nil)))))


(defun start-app (&key (port *port*))
  (bookshops.models:connect)
  (setf *server* (make-instance 'routes-acceptor :port port))
  (start *server*)
  (uiop:format! t "~&Application started on port ~a.~&" port))

(defun stop-app ()
  ;; disconnect db ?
  (stop *server*))
