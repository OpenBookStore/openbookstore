LISP?=sbcl

build:
	$(LISP) --load bookshops.asd \
                --load ../replic/replic.asd \
		--eval '(ql:quickload :bookshops)' \
		--eval '(asdf:make :bookshops)' \
		--eval '(quit)'

test:
	$(LISP) --load bookshops.asd \
                --load ../replic/replic.asd \
	        --load bookshops-test.asd \
	     --eval '(ql:quickload :bookshops)' \
	     --eval '(ql:quickload :bookshops-test)' \
	     --eval '(prove:run #p"tests/bookshops.lisp")' \
	     --eval '(quit)'
