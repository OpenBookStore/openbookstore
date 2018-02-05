build:
	sbcl --load bookshops.asd \
		--eval '(ql:quickload :bookshops)' \
		--eval '(asdf:make :bookshops)' \
		--eval '(quit)'
