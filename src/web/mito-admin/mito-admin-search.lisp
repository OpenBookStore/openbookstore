
(in-package :openbookstore.models)

(defun join-query-by-% (q)
  "Intersect \"%\" signs in the search query (string)
  so the SQL search is non exact.

  Example:

  (join-query-by-% \"foo bar\")
  ;; => \"%foo%bar%\""
  (if (str:non-blank-string-p q)
      (str:concat "%" (str:join "%" (str:words q)) "%")
      ""))

#+test-openbookstore
(and
 (assert (equal (join-query-by-% "foo")
                "%foo%"))
 (assert (equal (join-query-by-% "foo bar")
                "%foo%bar%"))
 (assert (equal (join-query-by-% "")
                "")))

;; Example working query:
#++
(mito:select-dao 'book
  (sxql:where
   (:or
    (:like 'title "%foo%")
    (:like 'name "%foo%")))
  (sxql:order-by (:desc :id)))

(defun %build-sxql-like-query (s fields)
  "S: the SQL-ready search term, prepared from the search query.
  FIELDS: list of field names (symbols or keywords).

  Example: S is \"%foo%\"."
  (loop for field in fields
        collect `(:like ,field ,s)))
#++
(%build-sxql-like-query "%foo%" '(:title :name))
;; ((:LIKE :TITLE "%foo%") (:LIKE :NAME "%foo%"))

(defun %join-or (clauses)
  "join list of clauses with a :OR.

  Useful to join the clauses of a text query together, when we don't need
to join them with another filter."
  `(:or
    ,@(loop for clause in clauses
         collect clause)))
#++(%join-or (%build-sxql-like-query "%foo%" '(:title :name)))
;; (:OR (:LIKE :TITLE "%foo%") (:LIKE :NAME "%foo%"))

;; You don't imagine the time I needed to figure this out ;) (years ago as a Lisp newcomer, but still)

(defun select-query-by-fields (table q fields)
  (let* ((order-by '(:desc :id))
         (fields (uiop:ensure-list fields))
         (search-field :name)
         (sql-query (join-query-by-% q))
         (records nil))
    (mito:select-dao table
      (sxql:where
       (%join-or
        (%build-sxql-like-query sql-query fields)))
      (sxql:order-by order-by))))
#++
(select-query-by-fields 'book "foo" '(title name))
;; (#<BOOK 121 - foo. SHELF: Histoire> #<BOOK 120 - foo. SHELF: NIL>)
;; #<SXQL-STATEMENT: SELECT * FROM book WHERE ((title LIKE '%foo%') OR (name LIKE '%foo%')) ORDER BY id DESC>

#+test-openbookstore
;; Searching for books with "lisp" in title should return results.
(assert (select-query-by-fields 'book "lisp" '(title name)))

(defgeneric search-records (table q)
  (:documentation "Search records in TABLE by the query Q (string)")
  (:method (table q)
    (let* ((order-by '(:desc :id))
           (search-field :name)
           (sql-query (join-query-by-% q))
           (records nil))
      (print "searching…")
      (setf records
            (select-query-by-fields table q (search-fields table)))
      (log:info records)
      records)))
