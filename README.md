# Bookshops

In development, don't look.

## Usage

    make build

    ./bookshops search terms

get a readline interactive prompt:

    ./bookshops -i
    bookshops > help

Available commands: `help`, `help help`, `search`,...


## Dev

Uses `replic` to build a readline interactive prompt (experimental):
https://github.com/vindarel/replic

Model usage:

```lisp
(in-package :bookshops.model)

(connect)

(make-book :title "antigone" :datasource "xxx")

(save-book *)

(find-dao 'book)
;; => #<Book antigone>
```

Slots: `title`... `quantity`, etc.

Troubleshooting:

- `DB is locked`: close and re-open: `(dbi:disconnect mito:*connection*)` and `(bookshops.model:connect)`. => [fixed upstream](https://github.com/fukamachi/mito/pull/28#issuecomment-377450798) ?
