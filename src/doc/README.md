OpenBookStore web client


# Searching books

The web interface currently allows:

- to search one ISBN
- to search by keyword.

When searching books in the "Search" menu, we get:

- the title, author(s), publisher, price, URL to the cover, date of publication
- if this book is already in our stock, how many copies we have.

## Dilicom

Dilicom is a french book data provider. We support its "FEL à la demande" web service. Support for the "FEL complet" is in study phase.

With "FEL à la demande" we can search by ISBN, and not do a free search.

We can't get the cover nor the summary either (those are additional commercial services), but we have additional information:

- the availability
- the VAT
- the CLIL theme
- the provider(s) (by its code)
- and more

# Quick search

On the menu bar, the input widget allows you to:

- type a few letters and have completion of the books you have in your stock
- scan **any** ISBN. If you don't have it in stock, we get it from a data source.


# Seeing one's stock

We can search our stock with the "Stock" menu.

We can click on a search result to see the book's own page.


# Adding books

On a book's page, use the "+1" (and even "-1") button.

See the **Receive menu** to scan books in a row (on development).

TODO: on search results, use similar buttons.
