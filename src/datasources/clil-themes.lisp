(defpackage bookshops.datasources.themes-clil
  (:use :cl)
  (:documentation "Mapping of CLIL themes, the book classification. We use it to guess the best shelf for a new book. Generate better mapping files."))

#|

parse-themes-file

Parses *themes-file*, data from Dilicom.

Print the hierarchy of themes on standard output:

level 1:    3000 -> SCOLAIRE
 level 2:   3001 -> SCOLAIRE -> Pré-scolaire et primaire
  level 3:  3002 -> SCOLAIRE -> Pré-scolaire et primaire -> Pré-scolaire et maternelle
  level 3:  3003 -> SCOLAIRE -> Pré-scolaire et primaire -> Élémentaire
 level 2:   3004 -> SCOLAIRE -> Manuels scolaires Secondaire Général
  level 3:  3005 -> SCOLAIRE -> Manuels scolaires Secondaire Général -> Collège
  level 3:  3006 -> SCOLAIRE -> Manuels scolaires Secondaire Général -> Lycée général
 …

Generate two files:

- theme-names.csv
  contains the mapping code -> name

- theme-hierarchies.csv
  contains the mapping code -> level 1 code -> … -> level 4 code
  (so the first code repeats itself once)

3000;3000;;;
3001;3000;3001;;
3002;3000;3001;3002;

|#

(in-package :bookshops.datasources.themes-clil)

(defparameter *themes-file* "themes-clil.csv"
  "File: from Dilicom's 319-TABLE DE CLASSIFICATION DES THEMES CLIL.xlsx, transformed to CSV, edited to remove the first comma of each line, that shouldn't be there.")

(defun parse-themes-file (&key (file *themes-file*) (start 0) end (write-to-files t))
  "For each theme number, associate the names of its level 1, 2, 3 and 4.

  Example:

  3744 -> 3722 Jeunesse / 3744 Fiction Jeunesse / <blank>

  3746 -> 3722 Jeunesse / 3744 Fiction Jeunesse / 3746 Histoire"
  (let* ((lines (uiop:read-file-lines file))
         current-l1
         current-l1-name
         current-l2
         current-l2-name
         current-l3
         current-l3-name
         current-l4
         current-l4-name)

    (with-open-file (theme-names "theme-names.csv"
                                 :direction :output
                                 :if-exists :supersede)
      ;; Hierarchies: theme -> parents
      ;; theme number ; level 1 ; level 2 ; level 2 ; level 4
      (with-open-file (theme-hierarchies "theme-hierarchies.csv"
                                         :direction :output
                                         :if-exists :supersede)
        (with-open-file (theme-hierarchies-names "theme-hierarchies-names.csv"
                                                 :direction :output
                                                 :if-exists :supersede)
          (loop for line in (subseq lines
                                    (or start 0)
                                    (or end (length lines)))
             for columns = (str:split ";" line)
             ;; Only one theme number is given on each line.
             for level-1 = (elt columns 0)
             for level-2 = (elt columns 1)
             for level-3 = (elt columns 2)
             for level-4 = (elt columns 3)
             for name = (elt columns 4)
             when (not (str:blankp level-1))
             do (progn (setf current-l1 level-1)
                       (setf current-l1-name name)
                       (format t "~%~a: ~a~&" level-1 name)
                       ;; Save to files.
                       (format theme-names "~&~a;~a" level-1 name)
                       ;; Try a yaml representation
                       (when write-to-files
                         (format theme-hierarchies-names "~&~a;~a" level-1 name)
                         (format theme-names "~&-~a;~a" level-1 name)
                         (format theme-hierarchies "~&~a;~a;;;" level-1 level-1)))

             when (not (str:blankp level-2))
             do (progn (setf current-l2 level-2)
                       (setf current-l2-name name)
                       (format t "~&~a: ~a / ~a~&" level-2 current-l1-name name)
                       (when write-to-files
                         (format theme-hierarchies-names "~&~a: ~a / ~a~&" level-2 current-l1-name name)
                         (format theme-names "~&~a;~a" level-2 name)
                         (format theme-hierarchies "~&~a;~a;~a;;" level-2 current-l1 level-2)))

             when (not (str:blankp level-3))
             do (progn (setf current-l3 level-3)
                       (setf current-l3-name name)
                       (format t "~&~a: ~a / ~a / ~a~&"
                               level-3
                               current-l1-name
                               current-l2-name
                               name)
                       (when write-to-files
                         (format theme-hierarchies-names "~&~a: ~a / ~a / ~a~&"
                                 level-3
                                 current-l1-name
                                 current-l2-name
                                 name)
                         (format theme-names "~&~a;~a" level-3 name)
                         (format theme-hierarchies "~&~a;~a;~a;~a;" level-3 current-l1 current-l2 level-3)))

             when (not (str:blankp level-4))
             do (progn (setf current-l4 level-4)
                       (setf current-l4-name name)
                       (format t "~&~a: ~a / ~a / ~a / ~a~&"
                               level-4
                               current-l1-name
                               current-l2-name
                               current-l3-name
                               name)
                       (when write-to-files
                         (format theme-names "~&~a;~a" level-4 name)
                         (format theme-hierarchies "~&~a;~a;~a;~a;~a"
                                 level-4 current-l1 current-l2 current-l3 level-4)))))))))
