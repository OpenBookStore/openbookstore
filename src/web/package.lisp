
;; the package is defined at the application root,
;; so than every file can refer to its symbol.
(in-package #:bookshops/web)
(djula:add-template-directory
 (asdf:system-relative-pathname "bookshops" "src/web/templates/"))
