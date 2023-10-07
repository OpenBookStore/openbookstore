#|
  This file is a part of bookshops project.
|#

(require "asdf")  ;; for CI
(require "uiop")  ;; for CI?
(require "cl+ssl")

(uiop:format! t "~&------- ASDF version: ~a~&" (asdf:asdf-version))
(uiop:format! t "~&------- UIOP version: ~a~&" uiop:*uiop-version*)

(asdf:defsystem "bookshops"
  :version "0.2"
  :author "vindarel"
  :license "GPL3"
  :description "Free software for bookshops."
  :homepage "https://gitlab.com/myopenbookstore/openbookstore/"
  :bug-tracker "https://gitlab.com/myopenbookstore/openbookstore/-/issues/"
  :source-control (:git "https://gitlab.com/myopenbookstore/openbookstore/")

  ;; Create .deb package.
  ;; :defsystem-depends-on ("linux-packaging")
  ;; :class "linux-packaging:deb"
  ;; :package-name "bookshops"

  :depends-on (
               ;; web client
               :dexador
               :plump
               :lquery
               :clss ;; might do with lquery only
               ;; DB
               :mito
               :dbd-sqlite3  ;; required for the binary (?)
               :mito-auth
               ;; readline
               :unix-opts
               :replic
               :cl-ansi-text
               :parse-number

               ;; utils
               :alexandria
               :can
               :function-cache          ;; scrapers
               :str
               :local-time
               :local-time-duration
               :cl-ppcre
               :parse-float
               :serapeum
               :log4cl
               :trivial-backtrace

               :deploy ;; must be a build dependency, but we also use deployed-p checks in our code.
               :fiveam  ;; dep of dep for build, required by deploy??

               ;; cache
               :cacle

               ;; web app
               :hunchentoot
               :easy-routes
               :djula
               :djula-gettext
               :cl-json
               :cl-slug

               :gettext
               )

  :components ((:module "src/datasources"
                        :components
                        ((:file "dilicom")
                         (:file "dilicom-flat-text")
                         (:file "base-scraper")
                         (:file "scraper-fr")
                         (:file "scraper-argentina")
                         ;; Depends on the above.
                         (:file "datasources")))

               (:module "src"
                        :components
                        ;; stand-alone packages.
                        ((:file "parameters")
                         (:file "utils")
                         (:file "i18n")
                         (:file "termp")
                         ;; they depend on the above.
                         (:file "packages")
                         (:file "authentication")
                         (:file "bookshops") ;; depends on src/web
                         (:file "database")))

               (:module "src/models"
                        :components
                        ((:file "shelves")
                         (:file "models")
                         (:file "models-utils")
                         (:file "baskets")
                         (:file "contacts")
                         (:file "sell")))

               ;; readline-based, terminal user-facing app (using REPLIC).
               (:module "src/terminal"
                        :components
                        ((:file "commands")
                         ;; relies on the above:
                         (:file "manager")))

               ;; One-off utility "scripts" to work on the DB.
               (:module "src/management"
                        :components
                        ((:file "management")))

               (:module "src/web"
                        :components
                        ((:file "package")
                         (:file "messages")
                         (:file "authentication")
                         (:file "search")
                         (:file "pre-web")
                         (:file "web")
                         (:file "api")))

               (:static-file "README.md")

               ;; Declare our templates to compile them in-memory, for delivery.
               (:module "src/web/templates"
                        :components
                        ;; Order is important: the ones that extend base.html
                        ;; must be declared after it, because we compile all of them
                        ;; at build time.
                        ((:static-file "login.html")
                         (:static-file "404.html")
                         (:static-file "base.html")
                         (:static-file "dashboard.html")
                         (:static-file "history.html")
                         (:static-file "loans.html")
                         (:static-file "search.html")
                         (:static-file "stock.html")
                         (:static-file "sell.html")
                         (:static-file "receive.html")
                         (:static-file "card-edit.html")
                         (:static-file "card-stock.html")
                         (:static-file "card-page.html")
                         (:static-file "card-create.html")
                         (:static-file "permission-denied.html")
                         (:static-file "no-nav-base.html")))

               ;; Static files (JS, CSS)
               (:module "src/static"
                        :components
                        ((:static-file "openbookstore.js")
                         (:static-file "card-page.js"))))

  ;; :build-operation "program-op"
  :entry-point "bookshops:run"
  :build-pathname "bookshops"

  ;; For a .deb (with the two lines above).
  ;; :build-operation "linux-packaging:build-op"

  ;; With Deploy, ship foreign libraries (and ignore libssl).
  :defsystem-depends-on (:deploy)  ;; (ql:quickload "deploy") before
  :build-operation "deploy-op"     ;; instead of "program-op"

  ;; :long-description
  ;; #.(read-file-string
  ;;    (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "bookshops-test"))))


;; Don't ship libssl, rely on the target OS'.
;; Needs to require or quickload cl+ssl before we can compile and load this .asd file? :S
#+linux (deploy:define-library cl+ssl::libssl :dont-deploy T)
#+linux (deploy:define-library cl+ssl::libcrypto :dont-deploy T)


;; Use compression: from 108M, 0.04s startup time to 24M, 0.37s.
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

;; Now ASDF wants to update itself and crashes.
;; On the target host, when we run the binary, yes. Damn!
;; Thanks again to Shinmera.
(deploy:define-hook (:deploy asdf) (directory)
  (declare (ignorable directory))
  #+asdf (asdf:clear-source-registry)
  #+asdf (defun asdf:upgrade-asdf () nil))
