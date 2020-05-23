#|
  This file is a part of bookshops project.
|#

(asdf:defsystem "bookshops"
  :version "0.1.1"
  :author "vindarel"
  :license "GPL3"
  :depends-on (
               ;; web client
               :dexador
               :plump
               :lquery
               :clss ;; might do with lquery only
               ;; DB
               :mito
               ;; readline
               :unix-opts
               :replic
               ;; utils
               :rutils
               :str
               :local-time
               :local-time-duration
               :cl-ppcre
               :parse-float

               ;; web app
               :hunchentoot
               :easy-routes
               :djula
               :cl-slug

               :log4cl
               :cl-i18n)
  :components ((:module "src/datasources"
                :components
                ((:file "dilicom")
                 (:file "scraper-fr")))

               (:module "src"
                :components
                ;; stand-alone packages.
                ((:file "parameters")
                 (:file "utils")
                 ;; they depend on the above.
                 (:file "packages")
                 (:file "database")
                 (:file "models")
                 (:file "models-utils")
                 (:file "baskets")
                 (:file "contacts")
                 (:file "bookshops")
                 (:file "commands")))

               ;; One-off utility "scripts" to work on the DB.
               (:module "src/management"
                :components
                ((:file "management")))

               (:module "src/web"
                :components
                ((:file "web")
                 (:file "api"))))

  :build-operation "program-op"
  :build-pathname "bookshops"
  :entry-point "bookshops:main"

  :description ""
  ;; :long-description
  ;; #.(read-file-string
  ;;    (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "bookshops-test"))))

;; from 108M, 0.04s startup time to 24M, 0.37s.
#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(asdf:defsystem "bookshops/gui"
  :version "0.1.0"
  :author "vindarel"
  :license "GPL3"
  :depends-on (:bookshops
               :nodgui)
  :components ((:module "src/gui"
                :components
                ((:file "gui"))))

  :build-operation "program-op"
  :build-pathname "bookshops-gui"
  :entry-point "bookshops.gui:main"

  :description "Simple graphical user interface to manage one's books."
  ;; :long-description
  ;; #.(read-file-string
  ;;    (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "bookshops-test"))))
