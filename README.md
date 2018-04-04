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

Troubleshooting:

- `DB is locked`: close and re-open: `(dbi:disconnect mito:*connection*)` and `(bookshops.model:connect)`. => fixed upstream ? (march)