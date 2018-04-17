# Bookshops

In development, don't look.

## Usage

    make build

    ./bookshops search terms

get a readline interactive prompt:

    ./bookshops -i
    bookshops > help

Available commands: `help`, `help help`

- `search`
- `add`
- `stock [keyword]`, with an optional keyword to filter by titles. Use `next` and `previous` for pagination.
- `details <i>`: print more information about the last search result
  number `<i>`. You can complete this argument using the TAB key
  (specially handy when you have filtered the results).

Parameters, to change with `set`:

- `*page-size*`


![](img.png)


## Dev

Uses `replic` to build a readline interactive prompt (experimental):
https://github.com/vindarel/replic

Model usage:

```lisp
(in-package :bookshops.model)
(use-package '(:mito :sxql))

(connect)

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
