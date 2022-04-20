(in-package :bookshops/web)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tools for web searches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Low level stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *isbn-search-cache*
  (cacle:make-cache 5000 '%isbn-search-datasources :test 'equal :lifetime (* 24 3600)))

(defvar *key-search-cache*
  (cacle:make-cache 5000 '%key-search-datasources :test 'equal :lifetime (* 24 3600)))

(defun %isbn-search-datasources (q)
  (declare (type string q))
  (values
   (if (dilicom:available-p)
       (dilicom:search-books (list q))
       (fr:books q))
   1))

(defun %key-search-datasources (q)
  (declare (type string q))
  (values (fr:books q) 1))

(declaim (ftype (function (string) models:list-of-search-results)
                remote-isbn-search remote-key-search))

(defun remote-isbn-search (q)
  (declare (type string q))
  (cacle:cache-fetch *isbn-search-cache* q))

(defun remote-key-search (q)
  (declare (type string q))
  (cacle:cache-fetch *key-search-cache* q))

;;; Note: the local functions return lists of book objects, not lists of hash tables.

(declaim (ftype (function (string) models:list-of-books) local-isbn-search local-key-search))

(defun local-isbn-search (q)
  (a:when-let ((res (models:find-by :isbn q)))
    (list res)))

(defun local-key-search (q)
  (models:find-book :query (bookshops.utils::asciify q)))

(declaim (ftype (function (hash-table &key (:save boolean)) models:book) save-remote-find))

(defun save-remote-find (find &key (save t))
  "Convert a remote search result into a book object and optionally save."
  (let*
      ((found find)
       (title (gethash :title found))
       (isbn (utils:clean-isbn (gethash :isbn found)))
       (authors (gethash :authors found ""))
       (price (gethash :price found ""))
       (price (utils:ensure-float price))
       (book (models:make-book :title title
                               :isbn isbn
                               :authors authors
                               :price price
                               :cover-url (access found :cover-url))))
    (when (and save book)
      (models:save-book book))
    book))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Higher level search funcs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun search-datasources (q)
  "Search on Dilicom if possible (ISBN only), otherwise search on the default datasource.
  After the search, we check if we have these books in our DB. If so, we augment their data with a `quantity' field.
  Results are cached for a day.
  QUERY can be an ISBN or keywords."
  (declare (type string q))
  (models::check-in-stock
   (cond
     ((bookshops.utils:isbn-p q) (remote-isbn-search q))
     ((not (str:blank? q)) (remote-key-search q))
     (t nil))))

(defvar *single-result* nil)

(declaim (ftype (function (string
                           &key
                           (:remote-key boolean) (:remote-isbn boolean) (:local-key boolean)
                           (:local-isbn boolean) (:save boolean))
                          models:list-of-books) get-or-search))

(defun get-or-search (q &key (remote-key t) (remote-isbn t)
                          (local-key t) (local-isbn t) save)
  "General search function. Will perform an ISBN or keyword search, both local and remote. ISBN searches will be performed before keyword, and local searches will be performed before remote. Search will stop on the first kind of search that returns a result.

Any of :remote-key, :remote-isbn, :local-key or :local-isbn may be set to NIL to stop that particular search. If results are found, a list of book objects will be returned.

Cards will be created for remote finds if :save is set T."
  (declare (type string q))
  (if (utils:isbn-p q)
      (a:if-let
          (found (and local-isbn (local-isbn-search q)))
        found
        (a:when-let*
            ((found (and remote-isbn (remote-isbn-search q)))
             (book (save-remote-find (car found) :save save)))
          (list book)))
      (a:if-let
          (found (and local-key (local-key-search q)))
        found
        (a:when-let*
            ((found (and remote-key (remote-key-search q)))
             (books (mapcar (lambda (b) (save-remote-find b :save save))
                            ;;Only save what we are going to return
                            (if *single-result* (list (car found)) found))))
          books))))

(declaim (ftype (function (string
                           &key
                           (:remote-key boolean) (:remote-isbn boolean) (:local-key boolean)
                           (:local-isbn boolean) (:save boolean))
                          models:book) get-or-search-single))

(defun get-or-search-single (q &key (remote-key t) (remote-isbn t)
                                 (local-key t) (local-isbn t) save)
  (let ((*single-result* t))
    (car (get-or-search q :remote-key remote-key :remote-isbn remote-isbn :local-key local-key
                      :local-isbn local-isbn :save save))))

(defun quick-search (q)
  "Either search an ISBN in our DB first, then on a datasource (and on that case, create a card object).
  either search by keywords in our DB.
  Return a plist:
  - :GO + card base URL if we got a result by ISBN
  - :RESULTS + JSON of books."
  (let ((res (get-or-search q :remote-key nil :save t)))
    (if (< 1 (length res))
        ;; Use strings as keys, because with symbols I have seen inconsistency
        ;; in the JSON results.
        ;; Once they are lowercased, once they are uppercased.
        ;; Switch from cl-json?
        (list "results"
              (mapcar (lambda (book)
                         (dict "url" (card-url book)
                                        "title" (models:title book)))
                      res))
        (when res
          (list "go" (card-url (car res)))))))

(defun sell-search (q)
  (let ((res (get-or-search q :remote-key nil :remote-isbn t :save t)))
    (cond
      ((< 1 (length res))
       (list "options"
             (mapcar (lambda (book)
                       ;; Embed the full book.
                       ;; It will be serialized by the JSON library.
                       (dict "card" book
                             "url" (card-url book)
                             "title" (models:title book)))
                     res)))
      ((eq 1 (length res))
       (list "card" (car res)))
      (t (list "error"
               (if (utils:isbn-p q)
                   "No book found for ISBN"
                   "No matches found in store"))))))
