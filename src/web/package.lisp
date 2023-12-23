
;; the package is defined at the application root,
;; so than every file can refer to its symbol.
(in-package #:openbookstore/web)
(djula:add-template-directory
 (asdf:system-relative-pathname "openbookstore" "src/web/templates/"))
