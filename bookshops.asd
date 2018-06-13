#|
  This file is a part of bookshops project.
|#

(asdf:defsystem "bookshops"
  :version "0.1.0"
  :author "vindarel"
  :license "MIT"
  :depends-on (
               ;; web
               :dexador
               :plump
               :lquery
               :clss ;; might do with lquery only
               ;; DB
               :mito
               ;; readline
               :unix-opts
               :replic ;; XXX not in QL
               ;; utils
               :str
               :log4cl
               :cl-i18n)
  :components ((:module "src"
                :components
                ((:file "utils")
                 (:file "models")
                 (:file "bookshops")
                 (:file "commands"))))

  :build-operation "program-op"
  :build-pathname "bookshops"
  :entry-point "bookshops:main"

  :description ""
  ;; :long-description
  ;; #.(read-file-string
  ;;    (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "bookshops-test"))))
