Web scrapers to books data
==========================

* the price must be returned as an integer, representing the price as
  cents (original price multiplied by 100).

  Use double floats, single floats get rounding errors very quickly
  (try `(* 100 9.90)` versus `9.90d0`).
