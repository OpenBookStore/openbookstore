(in-package :bookshops.models)

(defun create-ascii-slots ()
  "For all cards, create and save the ascii representations of: title, authors, publisher.
  (needs to migrate the table).
  To use manually when necessary."
  (time
   (loop for card in (bookshops.models::find-book)
      for i from 0
      for title-ascii = (asciify (title card))
      for authors-ascii = (asciify (authors card))
      for publisher-ascii = (asciify (publisher card))
      do (format t "- ~a asciify card ~a, \"~a\"~&"
                 i (object-id card) (str:shorten 40 (title card)))
      do (setf (bookshops.models::title-ascii card) title-ascii)
      do (setf (bookshops.models::authors-ascii card) authors-ascii)
      do (setf (bookshops.models::publisher-ascii card) publisher-ascii)
      do (save-dao card))))
