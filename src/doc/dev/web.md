# Web stack

* [Hunchentoot](https://github.com/edicl/hunchentoot) for web serving.
* [Easy-routes](https://github.com/mmontone/easy-routes) sits on top of hunchentoot.
* [Djula](https://quickref.common-lisp.net/djula.html), a Common Lisp port of the Python Django templating engine, is used for rendering.

# Getting book information

This is done either with Dilicom's "FEL à la demande" SOAP service, either with some web scraping.

# VueJS

## Variable interpolation with Djula and Vue

Both Djula and Vue use `{{ ... }}` for variable interpolation. To render Vue variables, use its built-in workaround: ``{$ {{ ... }} $}`.

# HTMX

https://htmx.org/

- `hx-boost` really makes pages snappy. It needs to grab a full-HTML result.
  We can use it, as of today:
  - on a card page, to pre-load a click on the shelf badge, that will show the query results nearly instantly.
  - on the navbar, to pre-load the new card form (doesn't use JS so far).
  - on the main menu: Stock, Search, History (so far at least). It is really great, pages take 5ms to display!!!

  However, pages that rely on JavaScript (Vanilla, Vue) can NOT be "boost"ed.
  - can't boost the Sell page :/

# Vue

Sell page, Reception page.

We need to add the entered ISBN on the page as soon as possible, and
wait for the server on the background.

I tried a bit to use HTMX websockets.
