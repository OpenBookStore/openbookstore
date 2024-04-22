
(in-package :mito-admin)

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

(defun select-query-by-fields (table q fields &key offset limit)
  (let* ((order-by '(:desc :id))
         (fields (uiop:ensure-list fields))
         (search-field :name)
         (sql-query (join-query-by-% q))
         (records nil))
    (mito:select-dao table
      (sxql:where
       (%join-or
        (%build-sxql-like-query sql-query fields)))
      (sxql:order-by order-by)
      (when limit
        (sxql:limit limit))
      (when offset
        (sxql:offset offset)))))
#++
(select-query-by-fields 'book "foo" '(title name))
;; (#<BOOK 121 - foo. SHELF: Histoire> #<BOOK 120 - foo. SHELF: NIL>)
;; #<SXQL-STATEMENT: SELECT * FROM book WHERE ((title LIKE '%foo%') OR (name LIKE '%foo%')) ORDER BY id DESC>

#+test-openbookstore
;; Searching for books with "lisp" in title should return results.
(assert (select-query-by-fields 'book "lisp" '(title name)))

(defparameter *page-size* 50) ;TODO: re-use other parameter.

(defgeneric search-records (table q &key page page-size)
  (:documentation "Search records in TABLE by the query Q (string)")
  (:method (table q &key page (page-size *page-size*))
    (let* ((order-by '(:desc :id))
           (search-field :name)
           (sql-query (join-query-by-% q))
           (offset (when page
                     (* (1- page) page-size)))
           (records nil))
      (print "searching…")
      (setf records
            (select-query-by-fields table q (search-fields table)
                                    :offset offset
                                    :limit page-size))
      (log:info records)
      records)))

(defun count-query-by-fields (table q fields)
  "Build our lax search query across those fields, but count the result only."
  ;; mito:count-dao is too simple, doesn't allow to filter.
  ;; https://github.com/fukamachi/mito/issues/110
  (let* ((order-by '(:desc :id))
         (fields (uiop:ensure-list fields))
         (sql-query (join-query-by-% q)))
    (cadr (first
           ;; result is like ((:|COUNT(*)| 6))
           (mito:retrieve-by-sql
            (sxql:select ((:count :*))
              (sxql:from table)
              (sxql:where
               (%join-or
                (%build-sxql-like-query sql-query fields)))
              (sxql:order-by order-by)))))))
#++
(progn
  (count-query-by-fields 'book "foo" '(title name))
  ;; 0 result with an author search??
  (count-query-by-fields 'book "sofocle" '(title name authors-ascii))
  )

(defgeneric count-records (table q)
  (:documentation "Filter results by the query Q and count the number of results.")
  (:method (table q)
    (count-query-by-fields table q (search-fields table))))
