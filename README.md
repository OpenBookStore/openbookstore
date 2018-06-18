# Bookshops

In development, don't look.

![](https://gitlab.com/vindarel/cl-bookshops/badges/master/pipeline.svg)

## Usage

    make build

    ./bookshops search terms

get a readline interactive prompt:

    ./bookshops -i
    bookshops > help

Available commands: `help`, `help help`

- `search`
- `add`
- `create`
- `delete`
- `stock [keyword]`, with an optional keyword to filter by titles. Use `next` and `previous` for pagination.
- `details <i>`: print more information about the last search result
  number `<i>`. You can complete this argument using the TAB key
  (specially handy when you have filtered the results).
- `inside`: print the current place we manipulate the books from. With an argument, change it (use TAB completion).
- `move`: move a book to another place. The place of origin is the one we are currently in (see `inside`). Give the quantity to move with for example "x2".
- `places`: lists the existing places with their number of books and their total cost.
- `stats`: some numbers. Percentage of books without isbn. With optional parameters to print the related results:
  - `noisbn`
- `quit`

When you are printed a list of results, use the pagination with `next` and `previous`.

Parameters, to change with `set`:

- `*page-size*`


![](img.png)

## i18n

This software is available in other languages than english.

See the makefile and `i18n-load` in `utils.lisp`.


## Dev

Uses `replic` to build a readline interactive prompt (experimental):
https://github.com/vindarel/replic

Model usage:

```lisp
(in-package :bookshops.model)
(use-package '(:mito :sxql))

(connect)
(i18n-load)

(make-book :title "antigone" :datasource "xxx")

(save-book *)

(find-dao 'book)
;; => #<Book antigone>

(select-dao 'book
    (where (:like :title "%ti%")))
```

Slots: `title`... `quantity`, etc.

### Testing

To test DB operations, use our macro `with-empty-db`.

(a clue it is working is that it should output migrations)

```lisp
(use-package :bookshops.models)
(use-package :bookshops-test.utils)

(with-empty-db
   (let* ((bk (make-book :title "inside-test")))
     (save-book bk)))

;;  CREATE TABLE "book" (
;;     "id" INTEGER PRIMARY KEY AUTOINCREMENT,
;;     "datasource" VARCHAR(128),
;;     "title" VARCHAR(128) NOT NULL,
;;     "price" INTEGER,
;;     "date_publication" VARCHAR(128),
;;     "editor" VARCHAR(128),
;;     "authors" VARCHAR(128),
;;     "quantity" INTEGER,
;;     "created_at" TIMESTAMP,
;;     "updated_at" TIMESTAMP
;; ) () [0 rows] | EXECUTE-SQL
#<BOOK inside-test>
```

### Troubleshooting

- `DB is locked`: close and re-open: `(dbi:disconnect mito:*connection*)` and `(bookshops.model:connect)`. => [fixed upstream](https://github.com/fukamachi/mito/pull/28#issuecomment-377450798) ?
