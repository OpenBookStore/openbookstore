build:
	sbcl --load bookshops.asd \
             --load ../replic/replic.asd \
		--eval '(ql:quickload :bookshops)' \
		--eval '(asdf:make :bookshops)' \
		--eval '(quit)'
