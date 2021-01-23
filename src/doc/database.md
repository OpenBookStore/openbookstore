# How the database is being created and accessed

## General

* The [Mito](https://github.com/fukamachi/mito) ([Quickref API](https://quickref.common-lisp.net/mito.html)) Common Lisp ORM is used.

## Code overview

(File paths are from within `src/`.)

* `database.lisp`: has the list of all tables (persisted classes)

    * defines procedures: `connect`, `ensure-tables-exist`, `bootstrap-base-roles`, `initialize-database`, `migrate-all`

* `authentication.lisp`, `models/{models,contacts,baskets}.lisp`: define the tables via `defclass` and mito's `mito:dao-table-mixin` metaclass. (NOTE: `mito:deftable` could be used to make definitions slightly shorter.)
