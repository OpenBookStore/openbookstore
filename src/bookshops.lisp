(defpackage bookshops
  (:use :cl
        :cl-ansi-text
        :parse-float
        :log4cl
        ;;
        :bookshops.parameters
        :bookshops.utils)
  (:shadow :search)
  (:import-from :bookshops.models
                :book
                :make-book
                :find-existing
                :title
                :publisher
                :authors
                :price
                :default-place
                :name)
  (:export :main
           :i18n-load
           :init
           ;; book accessors
           :publisher
           :title
           :authors
           :price
           ;; functions
           :books))
(in-package :bookshops)


(defparameter *french-search* "http://www.librairie-de-paris.fr/listeliv.php?MOTS={QUERY}&SUPPORT=&RECHERCHE=simple&TRI=&DISPOCHE=&RAYONS=&LIVREANCIEN=2&CSR="
  "French source of books. The {query} string will be replaced by the list
  of '+' separated search keywords.")

(defparameter *url-base* "http://www.librairie-de-paris.fr"
  ;; no trailing / for now plz.
  "Base url.")

(defparameter *datasource* *french-search*
  "The default data source.")

(defvar *last-results* nil
  "List of last results by `books` (objects).")

(defparameter *debug* nil)

(defun get-url (url)
  "Http get this url.
   Function mocked in unit tests."
  (dex:get url))

(defun parse (request)
  "Parse with plump, return a plump node.
   Function mocked in unit test."
  (plump:parse request))

(defun select (selector parsed)
  "Find nodes with CSS selector from a plump-parsed node."
  (lquery:$ selector parsed))

(lquery:define-lquery-list-function elt0 (vector)
  (elt vector 0))

(defun node-selector-to-text (selector node &key selector2)
  " Take a CSS selector (str), a plump node, extract and clean the result."
  (declare (ignorable selector2))
  (let* ((nodes (clss:select selector node))
         res
         txt)
    (setf nodes (coerce nodes 'list))
    (when (not (null nodes))
      (setf res (first nodes))
      (setf txt (plump:text res))
      (str:trim txt))))

(defmacro with-log-error ((name) &body body)
  `(handler-case
       (progn
         ,@body)
     (error (c) (format *error-output* "could not parse ~a: ~a." ,name c))))

(defun parse-title (node)
  (with-log-error (:title)
    (node-selector-to-text  ".livre_titre" node)))

(defun parse-authors (node)
  (with-log-error (:authors)
    (lquery:$ node ".livre_auteur a"
              (attr :title)
              (elt0))))

(defun parse-price (node)
  "Extract the price. `node': plump node."
  (extract-float (node-selector-to-text ".item_prix" node)))

(defun parse-publisher (node)
  (with-log-error (:publisher)
    (node-selector-to-text ".editeur" node)))

(defun parse-isbn (node)
  (str:trim (first (last (str:lines
                          (node-selector-to-text ".editeur-collection-parution" node))))))

(defun parse-publication-date (node)
  (with-log-error (:publication-date)
    (node-selector-to-text ".MiseEnLigne" node)))

(defun parse-cover-url (node)
  (with-log-error (:cover)
    (lquery:$ node ".zone_image img"
              (attr "data-original")
              (elt0))))

(defun parse-details-url (node)
  "Extract the url to the book online information.
  https://www.librairie-de-paris.fr/livre/9782742720682-antigone-henry-bauchau/"
  (with-log-error (:details-url)
    (str:concat *url-base*
                (lquery:$ node ".livre_titre a" (attr :href) (elt0)))))

(defun book-info (node)
  "Takes a plump node and returns a list of book objects with: title, authors, price, publisher, date of publication, etc."
  (let ((title (parse-title node))
        (authors (parse-authors node))
        (price (parse-price node))
        (publisher  (parse-publisher node))
        (date-publication (parse-publication-date node))
        (isbn (parse-isbn node))
        (cover-url (parse-cover-url node))
        (details-url (parse-details-url node))
        bk)
    (setf bk (make-book :title title
                        :isbn isbn
                        :datasource "fr:librairiedeparis"
                        :cover-url cover-url
                        :authors authors
                        :details-url details-url
                        :price price
                        :publisher publisher
                        :date-publication date-publication))
    (when *debug* (describe bk))
    ;; xxx: here we could make only 1 query to get the existing books (see Dilicom).
    ;; xxx: add in-stock field (see Dilicom).
    (find-existing bk :update t)))

(defun build-url (query &key (source *datasource*) (encode t))
  "Build the search url with the query terms in it.
  Encode the search terms (if `:encode' is true, the default).

  - query: a str (possibly many words).

  Return the url (a str).
  "
  (let* ((words (str:words query))
         (joined (str:join "+" words))
         (encoded? (if encode
                       (quri:url-encode joined)
                       joined)))
    (str:replace-all "{QUERY}" encoded? source)))

(defparameter *last-parsing-res* nil "for debug pursposes.")

(defun books (query &key (datasource *datasource*))
  "From a search query (str), return a list of book objects (with a title, a price, a date-publication, authors,...)."
  (declare (ignorable datasource))
  (unless mito::*connection* (bookshops.models:connect))
  (let* ((url (build-url query))
         (_ (log:info url))
         (req (get-url url))
         (parsed (parse req))
         ;; one node
         (node (clss:select ".resultsList" parsed))
         ;; many modes ;; vector, iterate with map
         ;; direct children:
         (res (clss:select "> li" node)))
    (declare (ignore _))
    (setf *last-parsing-res* (coerce res 'list))
    (setf *last-results* (map 'list #'book-info res))))

(defun init ()
  "Init i18n, connect to the DB,..."
  (bookshops.models:connect)
  (i18n-load)
  (log:config :error))

(defun handle-parser-error (c)
  (format t "Argument error: ~a~&" (opts:option c))
  ;; XXX: probably don't quit.
  (uiop:quit 1))

(defparameter +version+
  (let ((version (asdf/component:component-version (asdf:find-system :bookshops)))
        (directory (asdf:system-source-directory :bookshops)))
    (or (ignore-errors
          (uiop:with-current-directory (directory)
            (multiple-value-bind (current-commit)
                (uiop:run-program (list "git" "describe" "--always")
                                  :output '(:string :stripped t))
              (concatenate 'string
                           version
                           (format nil "-~a" current-commit)))))
        version))
  "The version number as in the asd appended with the current commit id.")

(defun main ()
  (opts:define-opts
      (:name :help
             :description "print this help and exit."
             :short #\h
             :long "help")

      (:name :version
             :description "print the version number and exit."
             :short #\v
             :long "version")

    (:name :interactive
           :description "enter the interactive prompt."
           :short #\i
           :long "interactive"))

  (multiple-value-bind (options free-args)
      (handler-bind ((error #'handle-parser-error))
        (opts:get-opts))

    (if (getf options :version)
        (progn
          (format t "~a~&" +version+)
          (uiop:quit)))

    (if (getf options :help)
        (progn
          (opts:describe)
          (uiop:quit)))

    (if (getf options :interactive)
        (progn
          (format t "Initializing...~&")
          (init)

          (setf replic:*prompt* (cl-ansi-text:green "bookshops > "))

          (setf replic:*prompt-prefix* (format nil "(~a) " (name (default-place))))

          ;; create commands from the exported functions and variables.
          (replic.completion:functions-to-commands :replic.base)

          (setf replic:*help-preamble* "With cl-bookshops you can search for books by keywords or isbn, add some to your stock and explore it.")
          (replic.completion:functions-to-commands :bookshops.commands)

          ;; define completions.
          ;; (push '("add" . *results*) replic:*args-completions*)

          (replic:repl))

        (handler-case
            (if free-args
                (books (str:join " " free-args)))
          (error (c)
            (progn
              (format *error-output* "~a~&" c)
              (uiop:quit 1)))))))
