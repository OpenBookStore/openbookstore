(defpackage bookshops
  (:use :cl
        :cl-ansi-text)
  (:shadow :search)
  (:import-from :bookshops.models
                :book
                :make-book
                :title
                :editor
                :authors
                :price)
  (:export :main
           ;; book accessors
           :editor
           :title
           :authors
           :price
           ;; functions
           :search))
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
  (handler-case
      (dex:get url)
    (error (c)
      (progn
        (format *error-output* c)))))

(defun parse (request)
  "Parse with plump, return a plump node.
   Function mocked in unit test."
  (plump:parse request))

(defun select (selector parsed)
  "Find nodes with CSS selector from a plump-parsed node."
  (lquery:$ selector parsed))

(defun node-selector-to-text (selector node)
  " Take a CSS selector (str), a plump node, extract and clean the result.
  "
  (let* ((nodes (clss:select selector node))
         (res (aref nodes 0))
         (txt (plump-dom:text res)))
    (str:trim txt)))

(defun book-info (it)
  "Takes a plump node and returns a list of book objects with: title, authors, price, publisher, date of publication, etc.
  "
  (let ((titre (node-selector-to-text  ".titre" it))
        (auteurs (node-selector-to-text ".auteurs" it))
        (prix  (node-selector-to-text ".prix_indicatif" it))
        (editeur  (node-selector-to-text ".editeur" it))
        (date-parution  (node-selector-to-text ".date_parution" it))
        ;; (href (node-selector-to-text ".titre[href]"))
        )
    ;; lesson learned: don't use make-instance 'book here, object will be different.
    (make-book :title titre
               :datasource "fr"
               :authors auteurs
               :price prix
               :editor editeur
               :date-publication date-parution)))

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
         (node (clss:select ".tab_listlivre" parsed))
         ;; many modes ;; vector, iterate with map
         (res (clss:select "tr" node)))
    (setf *last-results* (map 'list #'book-info res))))

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
          (setf replic:*prompt* (cl-ansi-text:green "bookshops > "))

          ;; create commands from the exported functions and variables.
          (replic:functions-to-commands :replic.base)

          (setf replic:*help-preamble* "With cl-bookshops you can search for books by keywords or isbn, add some to your stock and explore it.")
          (replic:functions-to-commands :bookshops.commands)

          ;; define completions.
          ;; (push '("add" . *results*) replic:*args-completions*)

          (bookshops.models:connect)
          (replic:repl))

        (handler-case
            (if free-args
                (search (str:join " " free-args)))
          (error (c)
            (progn
              (format *error-output* "~a~&" c)
              (uiop:quit 1)))))))
