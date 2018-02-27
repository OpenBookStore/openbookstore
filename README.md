# Bookshops

## Usage

    make build

    ./bookshops search terms

## Dev

Uses `replic` to build a readline interactive prompt (experimental):
https://github.com/vindarel/replic

Troubleshooting:

- `DB is locked`: close and re-open: `(dbi:disconnect mito:*connection*)` and `(bookshops.model:connect)`.