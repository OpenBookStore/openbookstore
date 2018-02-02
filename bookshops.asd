#|
  This file is a part of bookshops project.
|#

(asdf:defsystem "bookshops"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:dexador
               :plump
               :lquery
               :clss ;; might do with lquery only
               :str)
  :components ((:module "src"
                :components
                ((:file "bookshops"))))
  :description ""
  ;; :long-description
  ;; #.(read-file-string
  ;;    (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "bookshops-test"))))
