## Web scrapers

Get the HTML from a data source, return a list of hash-tables with book data:

- title, author(s), price, ISBN, URL to its page on the data source… (must)
- cover URL, publisher, publication date… (nice to have)

The price must be returned as an integer, representing the price as
cents (original price multiplied by 100).

Use double floats, single floats get rounding errors very quickly (try
`(* 100 9.90)` versus `9.90d0`).

### France


### Argentina

Create a scraper to cupsides.com

```lisp
(defparameter *ARGENTINA-SCRAPER* (make-instance 'scraper-argentina))
(books *ARGENTINA-SCRAPER* "antigona")
```

=>

```
((DICT
   :TITLE "Antigona"
   :ISBN NIL
   :DATASOURCE "Cuspide.com - Argentina"
   :COVER-URL "https://contentv2.tap-commerce.com/cover/medium/9789502309439_1.jpg?id_com=1113"
   :AUTHORS "Sòfocles"
   :DETAILS-URL "https://www.cuspide.com/Libro/9789502309439/Antigona"
   :PRICE 150000
   :PUBLISHER NIL
   :DATE-PUBLICATION NIL
  )
 (DICT
   :TITLE "Antigona"
   …))
```


Data sources for Argentina:

 - cuspide.com: looks complete enough.
   - TODO: doesn't show the ISBN inline in the search results, but it appears in the book's details URL, we can extract it.
   - doesn't show the publisher in search results. It appears on the book's details page.

 - https://www.libreriadonquijote.com.ar : no ISBN.

 - https://mercadolibre.com/ : to test.

## Create another books scraper

See `base-scraper.lisp`, its use in `scraper-argentina.lisp`.

It defines a base class with slots that represent:

- base URLs
- CSS selectors to get the title, the price…

and it defines default methods that help to parse the data. Hopefully
the default methods will help to extract data, but if necessary, a
scraper can override the necessary methods.
