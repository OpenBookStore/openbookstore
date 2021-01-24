# Web stack

* [Hunchentoot](https://github.com/edicl/hunchentoot) for web serving.
* [Easy-routes](https://github.com/mmontone/easy-routes) sits on top of hunchentoot.
* [Djula](https://quickref.common-lisp.net/djula.html), a Common Lisp port of the Python Django templating engine, is used for rendering.

# Getting book information

This is done either with Dilicom's "FEL à la demande" SOAP service, either with some web scraping.

# VueJS

## Variable interpolation with Djula and Vue

Both Djula and Vue use `{{ ... }}` for variable interpolation. To render Vue variables, use its built-in workaround: ``{$ {{ ... }} $}`.
