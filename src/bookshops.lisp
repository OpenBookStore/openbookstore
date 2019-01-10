(defpackage bookshops
  (:use :cl
        :bookshops.utils
        :cl-ansi-text
        :log4cl)
  (:shadow :search)
  (:import-from :bookshops.models
                :book
                :make-book
                :find-existing
                :title
                :editor
                :authors
                :price
                :default-place
                :name)
  (:export :main
           :i18n-load
           ;; book accessors
           :editor
           :title
           :authors
           :price
           ;; functions
           :books))
(in-package :bookshops)


(defparameter *french-search* "http://www.librairie-de-paris.fr/listeliv.php?MOTS={QUERY}&SUPPORT=&RECHERCHE=simple&TRI=&DISPOCHE=&RAYONS=&LIVREANCIEN=2&CSR="
  "French source of books. The {query} string will be replaced by the list
  of '+' separated search keywords.")

(defparameter *datasource* *french-search*
  "The default data source.")

(defvar *last-results* nil
  "List of last results by `books` (objects).")

(defun get-url (url)
  "Http get this url.
   Function mocked in unit tests."
  (drakma:http-request url))

(defun parse (request)
  "Parse with plump, return a plump node.
   Function mocked in unit test."
  (plump:parse request))

(defun select (selector parsed)
  "Find nodes with CSS selector from a plump-parsed node."
  (lquery:$ selector parsed))

(defun node-selector-to-text (selector node &key selector2)
  " Take a CSS selector (str), a plump node, extract and clean the result.
  "
  (declare (ignorable selector2))
  (let* ((nodes (clss:select selector node))
         res
         txt)
    (setf nodes (coerce nodes 'list))
    (when (not (null nodes))
      (setf res (first nodes))
      (setf txt (plump:text res))
      (str:trim txt))
    ))

(defun book-info (it)
  "Takes a plump node and returns a list of book objects with: title, authors, price, publisher, date of publication, etc.
  "
  (let ((titre (node-selector-to-text  ".livre_titre" it))
        (auteurs (node-selector-to-text ".livre_auteur" it))
        (prix  (node-selector-to-text ".prix_indicatif" it)) ;; .item_prix ?
        (editeur  (node-selector-to-text ".editeur" it))
        (date-parution  (node-selector-to-text ".date_parution" it))
        (isbn (str:trim (first (last (str:lines
                                      (node-selector-to-text ".editeur-collection-parution" it))))))
        (cover-url (aref (lquery-funcs:attr (clss:select "img" it)
                                            "src")
                         0))
        bk
        ;; (href (node-selector-to-text ".titre[href]"))
        )
    ;; lesson learned: don't use make-instance 'book here, object will be different.
    (setf bk (make-book :title titre
                        :isbn isbn
                        :datasource "fr"
                        :cover-url cover-url
                        :authors auteurs
                        :price prix
                        :editor editeur
                        :date-publication date-parution))
    (find-existing bk)))

(defun build-url (query &key (source *datasource*))
  "Build the search url with the query terms in it.

  - query: a str (possibly many words).

  Return the url (a str).
  "
  (let ((words (str:words query)))
    (str:replace-all "{QUERY}" (str:join "+" words) source)))

(defun books (query &key (datasource *datasource*))
  "From a search query (str), return a list of book objects (with a title, a price, a date-publication, authors,...).
  "
  (declare (ignorable datasource))
  (let* ((url (build-url query))
         (req (get-url url))
         (parsed (parse req))
         ;; one node
         (node (clss:select ".resultsList" parsed))
         ;; many modes ;; vector, iterate with map
         ;; direct children:
         (res (clss:select "> li" node)))
    (setf *last-results* (map 'list #'book-info res))))

(defun init ()
  "Init i18n, connect to the DB,..."
  (bookshops.models:connect)
  (i18n-load)
  (log:config :error))

(defun handle-parser-error (c)
  (format t "Argument error: ~a~&" (opts:option c))
  (uiop:quit 1))

(defun main ()

  (opts:define-opts
    (:name :help
           :description "print this help and exit."
           :short #\h
           :long "help")

    (:name :interactive
           :description "enter the interactive prompt."
           :short #\i
           :long "interactive"))

  (multiple-value-bind (options free-args)
      (handler-bind ((error #'handle-parser-error))
        (opts:get-opts))

    (if (getf options :help)
        (progn
          (opts:describe)
          (uiop:quit)))

    (if (getf options :interactive)
        (progn
          (init)

          (setf replic:*prompt* (cl-ansi-text:green "bookshops > "))
          (setf replic:*prompt-prefix* (format nil "(~a) " (name (default-place))))

          (format t "Initializing...~&")

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
