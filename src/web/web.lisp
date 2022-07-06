(in-package :bookshops/web)

#|
Show the stock on a web page.
(in construction)

(start-app)

and go to localhost:4242/stock/

In this file:
- custom Djula filters
- templates loading
- routes
- server start/stop

Dev helpers:
- adding ?raw=t on a URL (on a card page)

Slime reminders:
- use `C-u M-.` (`slime-edit-definition` with a prefix argument, also available as `M-- M-.`) to autocomplete the symbol and navigate to it. This command always asks for a symbol even if the cursor is on one. It works with any loaded definition. Here's a little [demonstration video](https://www.youtube.com/watch?v=ZAEt73JHup8).

|#

(defvar *server* nil
  "Current instance of easy-acceptor.")

(defparameter *port* 4242)

;;; Djula filters.
(djula:def-filter :price (val)
  (format nil "~,2F" val))

(defun card-url (card)
  "Create a full URL to uniquely identify this card."
  (declare (type models:book card))
  (format nil "/~a/~a-~a"
          "card"                       ; this can be translated
          (mito:object-id card)
          ;; the slug won't actually be read back, only the id.
          (slug:slugify (models:title card))))

(djula:def-filter :url (card)
  (card-url card))

(djula:def-filter :quantity (card)
  (typecase card
    (models:book (models:quantity card))
    (models:place-copies (models:place-copy-quantity card))
    (t (or (access:access card :in-stock) 0))))

(djula:def-filter :name (obj)
  (format nil "~a" (models:name obj)))

(djula:def-filter :describe (card)
  (with-output-to-string (s)
    (describe card s)))

;;; stolen options read-from-string idea from the djula time filter
(djula:def-filter :quantity-style (quantity raw-options)
  (let ((options (read-from-string raw-options)))
    (cond ((= 0 quantity) (access:access options :zero))
          ((plusp quantity) (access:access options :positive))
          (t (access:access options :negative)))))

(djula:def-filter :contact-name (contact-copy)
  ;; see why in loans.html. Mito limitation or me? Dec 2021.
  (if (and contact-copy
           (models::contact-copies-contact contact-copy))
      (format nil "~a" (models::name (models::contact-copies-contact contact-copy)))
      "?"))

;; If a load is outdated, show the due date in red.
(djula:def-filter :date-style (date raw-options)
  (let ((options (read-from-string raw-options)))
    (cond ((models::loan-too-long-p date) (access:access options :negative))
          (t (access:access options :positive)))))

;; Two filters necessary because buggy Mito which doesn't show the book
;; even though book-id is set (early 2021).
(djula:def-filter :print-contact (contact-id)
  (let ((contact (mito:find-dao 'models::contact :id contact-id)))
    (format nil "~a" (models::name contact))))

(djula:def-filter :print-book (book-id)
  (let ((obj (mito:find-dao 'models::book :id book-id)))
    (format nil "~a" (models::title obj))))

;;; Load templates.
(defparameter +base.html+ (djula:compile-template* "base.html"))
(defparameter +dashboard.html+ (djula:compile-template* "dashboard.html"))
(defparameter +search.html+ (djula:compile-template* "search.html"))
(defparameter +stock.html+ (djula:compile-template* "stock.html"))
(defparameter +card-page.html+ (djula:compile-template* "card-page.html"))
(defparameter +card-stock.html+ (djula:compile-template* "card-stock.html"))
(defparameter +card-create.html+ (djula:compile-template* "card-create.html"))
(defparameter +receive.html+ (djula:compile-template* "receive.html"))
(defparameter +sell.html+ (djula:compile-template* "sell.html"))
(defparameter +history.html+ (djula:compile-template* "history.html"))
(defparameter +loans.html+ (djula:compile-template* "loans.html"))

(defparameter +404.html+ (djula:compile-template* "404.html"))

;;;
;;; Serve static assets
;;;
(defparameter *default-static-directory* "src/static/"
  "The directory where to serve static assets from (STRING). If it starts with a slash, it is an absolute directory. Otherwise, it will be a subdirectory of where the system :abstock is installed.
  Static assets are reachable under the /static/ prefix.")

(defun serve-static-assets ()
  ;TODO: does not load our openbookstore.js file. Does it now ?
  (push (hunchentoot:create-folder-dispatcher-and-handler
         "/static/" (merge-pathnames *default-static-directory*
                                     (asdf:system-source-directory :bookshops)))
        hunchentoot:*dispatch-table*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some web utils.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun truthy-p (val)
  "Return t if VAL is truthy: it is t, 1, \"true\"…"
  (when (member val '(1 t "1" "t" "true" "yes") :test #'equal)
    t))
#+(or)
(assert
 (and (every #'truthy-p '(t "t" "1" 1 "true"))
      (not (every #'truthy-p '(t "f" "false")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Routes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bookshops.models:define-role-access home-route :view :visitor)
(defroute home-route ("/" :decorators ((@check-roles home-route))) ()
  ;; aka dashboard-route
  (render-template* +dashboard.html+ nil
                    :route "/"
                    :current-user (current-user)
                    :data (list :nb-titles (models:count-book)
                                :nb-books (models::total-quantities)
                                :nb-titles-negative (length
                                                     (models::negative-quantities))
                                :outdated-loans
                                (models::outdated-loans :limit 20 :order :asc)
                                :nb-outdated-loans
                                (models::count-outdated-loans)
                                )))

;XXX: we don't need a similar define-role-access for each route.
(bookshops.models:define-role-access stock-route :view :visitor)
(defroute stock-route ("/stock" :decorators ((@check-roles stock-route)))
    (&get q)
  (let ((cards (cond
                 ((utils:isbn-p q)
                  (list (models:find-by :isbn q)))
                 (q
                  (models:find-book :query (bookshops.utils::asciify q)))
                 (t
                  ;; XXX: pagination
                  (alexandria-2:subseq* (models:find-book) 0 50)))))
    (render-template* +stock.html+ nil
                      :route "/stock"
                      :title "Stock - OpenBookstore"
                      :cards cards
                      :nb-results (length cards)
                      :q q
                      :data (list :nb-titles (bookshops.models:count-book)
                                  :nb-books (bookshops.models::total-quantities)
                                  :nb-titles-negative (length
                                                       (bookshops.models::negative-quantities))))))

(bookshops.models:define-role-access search-route :view :visitor)
(defroute search-route ("/search" :decorators ((@check-roles search-route))) (&get q)
  (let ((cards (and q (search-datasources q))))
    (if cards
        (render-template* +search.html+ nil
                          :route "/search"
                          :title "Search - OpenBookstore"
                          :q q
                          :cards cards
                          :nb-results (length cards)
                          :title (format nil "OpenBookstore - search: ~a" q))
        (render-template* +search.html+ nil
                          :route "/search"
                          :title "Search - OpenBookstore"
                          :q q))))

(bookshops.models:define-role-access add-or-create-route :view :editor)
(defroute add-or-create-route ("/card/add-or-create/" :method :post
                                                      :decorators ((@check-roles add-or-create-route)))
    (q title isbn cover-url publisher (updatep :parameter-type 'boolean
                                               :init-form t)
       (book-id :parameter-type 'string :init-form "")
       (referer-route :parameter-type 'string :init-form "/search"))
  (let* ((book
          (if (str:blank? book-id)
              (models:find-existing
               (models:make-book :title title :isbn isbn :cover-url cover-url
                          :publisher publisher)
                             :update updatep)
              (models:find-by :id book-id))))
    (models:save-book book)
    (render-template* +card-page.html+ nil
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
           (mito:object-id book))))

(bookshops.models:define-role-access add-or-create-route :view :editor)
(defroute card-add-stock-route ("/card/add-stock/" :method :post
                                                   :decorators ((@check-roles add-or-create-route)))
    (q place-id (quantity :parameter-type 'integer :init-form 0) isbn
       (referer-route :parameter-type 'string :init-form "/search"))
  (let ((card (models:find-by :isbn isbn))
        (place (models:find-place-by :id place-id)))
    (bookshops.models:add-to place card :quantity quantity)
    (redirect-to-search-result referer-route q card)))

(bookshops.models:define-role-access add-or-create-route :view :editor)
(defroute card-quick-add-route ("/card/quick-add-stock/" :method :post
                                                         :decorators ((@check-roles add-or-create-route)))
    (q (quantity :parameter-type 'integer :init-form 1) title isbn cover-url publisher
       (updatep :parameter-type 'boolean :init-form t)
       (book-id :parameter-type 'string :init-form "")
       (referer-route :parameter-type 'string :init-form "/search"))
  (let ((book
         (if (str:blank? book-id)
             (models:find-existing
              (models:make-book :title title :isbn isbn :cover-url cover-url
                                :publisher publisher)
              :update updatep)
             (models:find-by :id book-id))))
    (models:save-book book)
    (bookshops.models:add-to (models:default-place) book :quantity quantity)
    (redirect-to-search-result referer-route q book)))

(bookshops.models:define-role-access add-or-create-route :view :visitor)
(defroute card-page ("/card/:slug" :decorators ((@check-roles add-or-create-route)))
    (&get raw)
  "Show a card.

  If the URL parameter RAW is \"t\" (the string), then display the card object literally (with describe)."
  (let* ((card-id (ignore-errors
                    (parse-integer (first (str:split "-" slug)))))
         (card (when card-id
                 (models:find-by :id card-id))))
    (cond
      ((null card-id)
       (render-template* +404.html+ nil))
      (card
       (render-template* +card-stock.html+ nil
                         :messages nil
                         :route "/stock"
                         :card card
                         :places-copies (models::book-places-quantities card)
                         :raw raw))
      (t
       (render-template* +404.html+ nil)))))

(bookshops.models:define-role-access card-create-route :view :editor)
(defroute card-create-route ("/card/create" :method :get
                                            :decorators ((@check-roles card-create-route)))
    ()
  (describe (hunchentoot:start-session) t) ;; XXX debug
  ;; (log:info (bookshops.messages::add-message "Hello message :)"))
  (render-template* +card-create.html+ nil
                    :messages/status (bookshops.messages:get-message/status)))

(defroute card-create/post-route ("/card/create" :method :post
                                                 :decorators ((@check-roles card-create-route)))
    ;; title is mandatory, the rest is optional.
    (title isbn price authors)
  (when (str:blankp title)
    (bookshops.messages::add-message "Please enter a title" :status :warning)
    (hunchentoot:redirect "/card/create"))
  ;XXX: handle more than one validation message.
  (when (and (str:non-blank-string-p isbn)
             (not (bookshops.utils:isbn-p isbn)))
    (bookshops.messages::add-message (format nil "This doesn't look like an ISBN: ~a" isbn) :status :warning)
    (hunchentoot:redirect "/card/create"))
  (handler-case
      (let ((book (models:make-book :title title
                                    :isbn (bookshops.utils:clean-isbn isbn)
                                    :authors authors
                                    :price (utils:ensure-float price))))
        (mito:save-dao book)
        (bookshops.messages:add-message "The book was created succesfully.")
        (hunchentoot:redirect "/card/create"))
    (error (c)
                                        ;XXX: 404 handled by hunchentoot
      (format *error-output* c))))

(bookshops.models:define-role-access receive-route :view :editor)
(defroute receive-route ("/receive" :method :get
                                    :decorators ((@check-roles receive-route)))
    ()  ;; args
  (render-template* +receive.html+ nil
                    :route "/receive"
                    :title "Receive - OpenBookstore"))

(bookshops.models:define-role-access sell-route :view :editor)
(defroute sell-route ("/sell" :method :get
                              :decorators ((@check-roles sell-route)))
    ()  ;; args
  (render-template* +sell.html+ nil
                    :route "/sell"
                    :title "Sell - OpenBookstore"))

(bookshops.models:define-role-access history-route :view :editor)
(defroute history-route ("/history" :decorators ((@check-roles history-route))
                                    :method :get)
    ()  ;; args
  (render-template* +history.html+ nil
                    :route "/history"
                    :title "History - OpenBookstore"
                    :soldcards (models::find-soldcards :order :desc)))

(bookshops.models:define-role-access loans-route :view :editor)
(defroute loans-route ("/stock/loans" :decorators ((@check-roles history-route))
                                      :method :get)
    (outdated)  ;; args
  (setf outdated (truthy-p outdated))
  (render-template* +loans.html+ nil
                    :route "/stock/loans"
                    :title "Loans - OpenBookstore"
                    :loans (if outdated
                               (models::outdated-loans)
                               (models::find-loans))
                    :filter-outdated-p outdated))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start-up functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-app (&key (port *port*))
  (bookshops.models:connect)

  ;; fix a puri bug. puri:parse-uri "/login?referer-route=/stock?q=lisp" fails,
  ;; it doesn't like the last ?. See https://gitlab.common-lisp.net/clpm/puri/-/issues/2
  (setf puri::*strict-illegal-query-characters*
        (remove #\? puri::*strict-illegal-query-characters*))

  ;; Watch out case output. This trips up JS.
  ;; The output of
  #+(or) (cl-json:encode-json-plist-to-string (sell-search "test"))
  ;; should be encoded in lowercase.
  ;; This is the default but it has been a source of error already.
  (setf json:*json-identifier-name-to-lisp* #'json:camel-case-to-lisp)
  (setf json:*lisp-identifier-name-to-json* #'json:lisp-to-camel-case)

  (setf *server* (make-instance 'easy-routes:easy-routes-acceptor :port port))
  (hunchentoot:start *server*)
  (serve-static-assets)
  (uiop:format! t "~&Application started on port ~a.~&" port))

(defun stop-app ()
  ;; disconnect db ?
  (hunchentoot:stop *server*))

;;;;;;;;;;;;;;;
; Dev helpers ;
;;;;;;;;;;;;;;;
(defun set-devel-profile ()
  "- interactive debugger"
  (setf hunchentoot:*catch-errors-p* nil))
