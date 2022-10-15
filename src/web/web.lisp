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

(djula:def-filter :slugify (s)
  (slug:slugify s))

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

(djula:def-filter :background-color (loop-index)
  "Alternate between white and grey color background in a Djula loop.
  For list of shelves."
  (if (zerop (mod loop-index 2))
      "white"
      "#eee9e9"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Templates.
;;
;; Before compiling them with Djula, we tell Djula to compile them in-memory.
;; This is for binary releases.
;;
;; But for development, we like them to be on filesystem and to be re-read on change.
;; See SET-DEVEL-PROFILE.
;;
;; So, we grab all our templates defined in the .asd and compile them in-memory.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf djula:*current-store*
      (make-instance 'djula:memory-template-store
		     :search-path (list (asdf:system-relative-pathname "bookshops"
                                                                       "src/web/templates/"))))

(let ((paths (djula:list-asdf-system-templates "bookshops" "src/web/templates")))
  (loop for path in paths
     do (uiop:format! t "~&Compiling template file in memory: ~a…~&" path)
       (djula:compile-template* path))
  (values t :all-done))

;; Release setting: don't re-compile templates on change.
;; However we NEED this for development, see SET-DEVEL-PROFILE.
(format t "~&NOTE: preventing Djula to recompile templates on change… set djula:*recompile-templates-on-change* to T for development (see SET-DEVEL-PROFILE).~&")
(setf djula:*recompile-templates-on-change* nil)

(uiop:format! t "~&All templates compiled in memory.~&")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compile and load templates (as usual).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter +base.html+ (djula:compile-template* "base.html"))
(defparameter +dashboard.html+ (djula:compile-template* "dashboard.html"))
(defparameter +search.html+ (djula:compile-template* "search.html"))
(defparameter +stock.html+ (djula:compile-template* "stock.html"))
(defparameter +card-page.html+ (djula:compile-template* "card-page.html"))
(defparameter +card-stock.html+ (djula:compile-template* "card-stock.html"))
(defparameter +card-create.html+ (djula:compile-template* "card-create.html"))
(defparameter +card-update.html+ (djula:compile-template* "card-edit.html"))
(defparameter +receive.html+ (djula:compile-template* "receive.html"))

;; from authentication.lisp
(defparameter +no-nav-base.html+ (djula:compile-template* "no-nav-base.html"))
(defparameter +permission-denied.html+ (djula:compile-template* "permission-denied.html"))
(defparameter +login.html+ (djula:compile-template* "login.html"))

;; Testing the UI with HTMX websockets
(defparameter +receive-ws.html+ (djula:compile-template* "receive-ws.html"))

(defparameter +sell.html+ (djula:compile-template* "sell.html"))
(defparameter +history.html+ (djula:compile-template* "history.html"))
(defparameter +loans.html+ (djula:compile-template* "loans.html"))

(defparameter +404.html+ (djula:compile-template* "404.html"))

;;;
;;; Serve static assets
;;;
;;; Two possibilities:
;;; - the app is run from source: use the usual create-folder-dispatcher-and-handler
;;; - the app is built for a standalone binary: we must get the static files content at compilation and serve them later. Hunchentoot must not access the file system, it won't find the files anymore.

;;
;; 1) The app is run from sources.
;;

;; *default-static-directory* is defined earlier in pre-web.lisp

(defun serve-static-assets ()
  "Serve static assets under the /src/static/ directory when called with the /static/ URL root.
  See serve-static-assets-for-release to use in a binary release."
  (push (hunchentoot:create-folder-dispatcher-and-handler
         "/static/" (merge-pathnames *default-static-directory*
                                     (asdf:system-source-directory :bookshops) ;; => NOT src/
                                     ))
        hunchentoot:*dispatch-table*))

;;
;; 2) We build a binary and we want to include static files.
;;

;; see pre-web.lisp
;; Because we use the #. reader macro, we need to define some things in another file,
;; that is loaded when this one is loaded.

(defun serve-static-assets-for-release ()
  "In a binary release, Hunchentoot can not serve files under the file system: we are on another machine and the files are not there.
  Hence we need to get the content of our static files into memory and give them to Hunchentoot."
  (push
   (hunchentoot:create-regex-dispatcher "/static/openbookstore\.js"
                                        (lambda ()
                                          ;; Returning the result of the function calls silently fails. We need to return a string.
                                          ;; Here's the string, read at compile time.
                                          #.(%serve-static-file "openbookstore.js")))
   hunchentoot:*dispatch-table*)

  (push
   (hunchentoot:create-regex-dispatcher "/static/card-page\.js"
                                        (lambda ()
                                          #.(%serve-static-file "card-page.js")))
   hunchentoot:*dispatch-table*))

#+#:only-for-devel
(setf hunchentoot:*dispatch-table* nil)

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
    (&get q
          (shelf-name-ascii-or-id :real-name "shelf"))
  (let* ((shelf (cond
                  ;; We preferably create links with shelf ascii names.
                  ;; but in the card page JS, we edit the link with the id… simpler for JS side.
                  ((integerp
                    (ignore-errors (parse-integer shelf-name-ascii-or-id)))
                   (models::find-shelf-by :id shelf-name-ascii-or-id))
                  (t
                   (log:info shelf-name-ascii-or-id)
                   (models::find-shelf-by :name-ascii shelf-name-ascii-or-id))))

         ;; Had a doubt if the search returned a list…
         ;; (shelf (if (and shelves (listp shelves)) ;; unsure… see find-by (for books) and this.
         ;;            (first shelves)
         ;;            shelves))
         (cards (cond
                  ((utils:isbn-p q)
                   (list (models:find-by :isbn q)))
                  (q
                   (models:find-book :query (bookshops.utils::asciify q)
                                     :shelf shelf))
                  (t
                   ;; XXX: pagination
                   (alexandria-2:subseq* (models:find-book :shelf shelf) 0 50)))))
    (render-template* +stock.html+ nil
                      :route "/stock"
                      :title (format nil "Stock ~a~a ~a - OpenBookstore"
                                     (if q q "")
                                     (if  shelf ", " "")
                                     (if shelf (models::name shelf) ""))
                      :cards cards
                      :shelves (models::find-shelf)
                      :form-shelf shelf
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

(defroute search-route/post ("/search" :method :POST) (&post q)
  (let ((cards (and q (search-datasources q))))
    (if cards
        (str:concat
         "<div class=\"dropdown-content\">"
         "<div class=\"dropdown-item\" href=0> Beer name here </div>"
         "<div class=\"dropdown-item\" href=0>
          <p> Other beer name <bold> here </bold>
           </div>"
         "</div>"))))

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

;; Card view.
(bookshops.models:define-role-access add-or-create-route :view :visitor)
(defroute route-card-page ("/card/:slug" :method :GET :decorators ((@check-roles add-or-create-route)))
    (&get raw)
  "Show a card.

  If the URL parameter RAW is \"t\" (the string), then display the card object literally (with describe)."
  (let* ((card-id (ignore-errors
                    (parse-integer (first (str:split "-" slug)))))
         (card (when card-id
                 (models:find-by :id card-id)))
         (shelves (models::find-shelf)))
    (cond
      ((null card-id)
       (render-template* +404.html+ nil))
      (card
       (render-template* +card-stock.html+ nil
                         :messages nil
                         :card card
                         :places-copies (models::book-places-quantities card)
                         :shelves shelves
                         :raw raw))
      (t
       (render-template* +404.html+ nil)))))

;; Carde create: GET.
(models:define-role-access card-create-route :view :editor)
(defroute card-create-route ("/card/create" :method :get
                                            :decorators ((@check-roles card-create-route)))
    (title isbn price authors shelf-id new-shelf-name)
  ;; see also: the API for POST updates.
  ;; (describe (hunchentoot:start-session) t)
  ;; (log:info (bookshops.messages::add-message "Hello message :)"))
  ;;
  ;; If we have URL params, that means we tried to submit a form,
  ;; and got a validation error, so we want to re-show the data submitted.
  ;; Like a form does.
  ;; But… we don't show erroneous fields in red and we do very little client side validation…
  (let ((card (models:make-book :title title
                                :isbn isbn
                                :price price
                                :authors authors
                                :shelf-id shelf-id))
        ;; More data we want to pass along to the form.
        ;; so actually… we don't really need the card object, only
        ;; a big form-data.
        (more-form-data (dict :new-shelf-name new-shelf-name)))
    (render-template* +card-create.html+ nil
                      :shelves (models::find-shelf)
                      :title "New book - OpenBookstore"
                      :card card
                      :more-form-data more-form-data
                      :messages/status (bookshops.messages:get-message/status))))

;; Carde create: POST.
(models:define-role-access card-create/post-route :view :editor)
(defroute card-create/post-route ("/card/create" :method :post
                                                 :decorators ((@check-roles card-create-route)))
    ;; title is mandatory, the rest is optional.
    (title isbn price authors shelf-id new_shelf_name)

  (when (str:blankp title)
    (bookshops.messages::add-message "Please enter a title" :status :warning)
    (hunchentoot:redirect "/card/create"))

  ;; XXX: handle more than one validation message.
  (when (and (str:non-blank-string-p isbn)
             (not (bookshops.utils:isbn-p isbn)))
    (bookshops.messages::add-message (format nil "This doesn't look like an ISBN: ~a" isbn) :status :warning)
    (hunchentoot:redirect
     ;; Redirect to the create page, give the pre-entered data as URL params.
     ;; We could also use session data.
     (easy-routes:genurl 'card-create/post-route
                         :title title
                         :isbn isbn
                         :price price
                         :authors authors
                         :shelf-id shelf-id
                         :new-shelf-name new_shelf_name)))
  (handler-case
      (let ((book (models:make-book :title title
                                    :isbn (bookshops.utils:clean-isbn isbn)
                                    :authors authors
                                    :shelf-id shelf-id
                                    :new-shelf-name new_shelf_name
                                    :price (utils:ensure-float price))))
        (mito:save-dao book)
        (bookshops.messages:add-message "The book was created succesfully.")
        (hunchentoot:redirect "/card/create"))
    (error (c)
      ;; XXX: 404 handled by hunchentoot
      (format *error-output* "An unexpected error happened: ~a" c)
      (error c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Edit/update an existing card.
;;; Similar from create.
(defroute card-update-route ("/card/update/:id" :method :get
                                                :decorators ((@check-roles card-create-route)))
    ()
  ;; see also: the API for POST updates.
  ;; (describe (hunchentoot:start-session) t)
  ;; (log:info (bookshops.messages::add-message "Hello message :)"))
  (let ((card (models::find-by :id id)))
    (render-template* +card-update.html+ nil
                      :card card
                      :shelves (models::find-shelf)
                      :title (format nil "Edit: ~a - OpenBookstore" (str:shorten 40 (models:title card) :ellipsis "…"))
                      :messages/status (bookshops.messages:get-message/status))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; card update POST
(models:define-role-access card-update/post-route :view :editor)
(defroute card-update/post-route ("/card/update/:id" :method :post
                                                 :decorators ((@check-roles card-create-route)))
    ;; title is mandatory, the rest is optional.
    (title isbn price authors shelf-id)

  ;; Begin form validation.
  (when (str:blankp title)
    (bookshops.messages::add-message "Please enter a title" :status :warning)
    (hunchentoot:redirect "/card/create"))

  ;XXX: handle more than one validation message.
  (when (and (str:non-blank-string-p isbn)
             (not (bookshops.utils:isbn-p isbn)))
    (bookshops.messages::add-message (format nil "This doesn't look like an ISBN: ~a" isbn) :status :warning)

    (let ((card (models::find-by :id id)))
      (setf card (models::update-book-with card
                                             `((:title ,title)
                                               (:isbn ,isbn)
                                               (:price ,price)
                                               (:authors ,authors))))
      (return-from card-update/post-route
        (render-template* +card-update.html+ nil
                          :card card
                          :shelves (models::find-shelf)
                          :title (format nil "Edit: ~a - OpenBookstore" (str:shorten 40 (models:title card) :ellipsis "…"))
                          :messages/status (bookshops.messages:get-message/status)))))

  ;; Form validation OK.
  (handler-case
      (let ((book (models:find-by :id id))
            (shelf (models::find-shelf-by :id shelf-id)))
        ;; Update fields.
        (setf book (models::update-book-with book
                                             `((:title ,title)
                                               (:isbn ,isbn)
                                               (:price ,price)
                                               (:authors ,authors)
                                               (:shelf ,shelf))))

        ;; Save.
        (mito:save-dao book)
        ;; We don't see the message after a redirect, too bad.
        ;; (bookshops.messages:add-message "The book was updated succesfully.")
        (hunchentoot:redirect (easy-routes:genurl 'route-card-page
                                                  :slug (str:concat id "-" (slug:slugify title)))))
    (error (c)
      ;; XXX: 404 handled by hunchentoot
      (format *error-output* c))))


(bookshops.models:define-role-access receive-route :view :editor)
(defroute receive-route ("/receive" :method :get
                                    :decorators ((@check-roles receive-route)))
    ()  ;; args
  (render-template* +receive.html+ nil
                    :route "/receive"
                    :shelves (models::find-shelf)
                    :title "Receive - OpenBookstore"))

(defroute receive-in-shelf-route ("/receive/:shelf-slug" :method :get
                                    :decorators ((@check-roles receive-route)))
    () ;; args
  (let ((shelf (models::find-shelf-by :id
                                      (first (str:split "-" shelf-slug)))))
    (log:info shelf "request URI?" (hunchentoot:request-uri*))
    (render-template* +receive.html+ nil
                      :route "/receive"
                      :current-shelf shelf
                      :shelves (models::find-shelf)
                      :title "Receive - OpenBookstore")))

(defroute receive-ws-route ("/receive-ws" :method :get
                                    :decorators ((@check-roles receive-route)))
    () ;; args
  (render-template* +receive-ws.html+ nil
                    :route "/receive-ws"
                    :title "Receive WS Devel- OpenBookstore"))

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
  (let ((data (models::group-sells-and-soldcards :order :desc
                                                 :min-date (utils::x-days-ago 30)
                                                 :max-date (local-time:today))))
    (render-template* +history.html+ nil
                      :route "/history"
                      :title "History - OpenBookstore"
                      :data data)))

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
        ;; xxx: mmmh… not enough (any more)
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
  (if (deploy:deployed-p)
      ;; Binary release: don't serve files by reading them from disk.
      (serve-static-assets-for-release)
      ;; Normal setup, running from sources: serve static files as usual.
      (serve-static-assets))
  (uiop:format! t "~&Application started on port ~a.~&" port))

(defun stop-app ()
  ;; disconnect db ?
  (hunchentoot:stop *server*))

;;;;;;;;;;;;;;;
;; Dev helpers
;;;;;;;;;;;;;;;
(defun set-devel-profile ()
  "- interactive debugger
   - re-read Djula templates on change (maybe not enough, see in-memory template compilation)."
  (log:config :debug)
  (setf djula:*recompile-templates-on-change* t)
  (setf hunchentoot:*catch-errors-p* nil))
