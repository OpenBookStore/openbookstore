LISP ?= sbcl
# SHELL := bash

# List lisp files, unless they contains a #
SRC := $(shell find src/ -name '*.lisp' -a ! -name '*#*')
HTML := $(shell find src/ -name '*.html' -a ! -name '*#*')
DEPS := $(SRC) $(HTML) bookshops.asd # and some more...

# list of supported locales
LOCALES := fr_FR
# Example of how the variable should look after adding a new locale:
# LOCALES := fr_FR en_GB

# list of .po files (computed from the LOCALES variable)
PO_FILES := $(foreach locale,$(LOCALES),locale/$(locale)/LC_MESSAGES/bookshops.po)
# list of .mo files (computed from the LOCALES variable)
MO_FILES := $(foreach locale,$(LOCALES),locale/$(locale)/LC_MESSAGES/bookshops.mo)

all: test

run:
	rlwrap $(LISP) --load run.lisp

build: ${MO_FILES}
	# quickload cl+ssl: required to load the .asd, for Deploy.
	# Don't reload templates and use a memory store.
	#
	# I use a feature flag, bc using djula:*recompile-templates-on-change*
	# requires to load Djula before our app, and it isn't exactly the same meaning.
	$(LISP)	--non-interactive \
		--eval '(ql:quickload "deploy")' \
		--eval '(ql:quickload "cl+ssl")' \
		--load bookshops.asd \
		--eval '(push :djula-binary *features*)' \
		--eval '(ql:quickload :bookshops)' \
		--eval '(asdf:make :bookshops)'

build-package:
	# This must use a custom-built SBCL with a special parameter,
	# see linux-packaging README.
	# I have it under ~/.local/bin/bin/sbcl
	$(LISP) --non-interactive \
		--load bookshops.asd \
		--eval '(ql:quickload :bookshops)' \
		--eval '(load "~/common-lisp/asdf/tools/load-asdf.lisp")' \
		--eval '(setf *debugger-hook* (lambda (c h) (declare (ignore h)) (format t "~A~%" c) (sb-ext:quit :unix-status -1)))' \
		--eval '(asdf:make :bookshops)'

build-gui:
	$(LISP)	--non-interactive \
		--load bookshops.asd \
		--eval '(ql:quickload :bookshops/gui)' \
		--eval '(asdf:make :bookshops/gui)'

docker-build:
	sudo docker build . -t openbookstore

docker-run:
	sudo docker run -it --rm --name my-obs openbookstore


test:
	$(LISP) --non-interactive \
		--load bookshops.asd \
		--load bookshops-test.asd \
		--eval '(ql:quickload :bookshops-test)' # prove tests are run when loaded.

# Install dependencies, mostly for docker (gitlab CI).
install:
	# we might need the latest.
	git clone https://github.com/vindarel/replic/ ~/quicklisp/local-projects/replic/
	git clone https://github.com/vindarel/cl-str/ ~/quicklisp/local-projects/cl-str/


# Workflow:
# - update
# - compile
# It seems we can not parse messages with the #! prefix, as does cl-i18n with its lisp storage.
# Needed to set the charset to UTF-8 in the pot manually.
# http://www.labri.fr/perso/fleury/posts/programming/a-quick-gettext-tutorial.html
#
# lisp.pot contains the string to translate that were extracted from
# the .lisp files.
#
# djula.pot contains the strings to transtlate that were extracted
# from the djula templates.


.PHONY: tr
tr: ${MO_FILES}

PO_TEMPLATE_DIR := locale/templates/LC_MESSAGES
PO_TEMPLATE := ${PO_TEMPLATE_DIR}/bookshops.pot

# Rule to extract translatable strings from SRC
${PO_TEMPLATE_DIR}/lisp.pot: $(SRC)
	mkdir -p $(@D)
	xgettext -k_ -kN_ --language=lisp -o $@ $^

# Rule to extract translatable strings from djula templates
${PO_TEMPLATE_DIR}/djula.pot: $(HTML) src/i18n.lisp
	$(LISP) --non-interactive \
		--eval '(ql:quickload "deploy")' \
		--eval '(ql:quickload "cl+ssl")' \
		--eval '(asdf:load-asd (truename "bookshops.asd"))' \
		--eval '(push :djula-binary *features*)' \
		--eval '(ql:quickload :bookshops)' \
		--eval '(bookshops.i18n:update-djula.pot)'

# Rule to combine djula.pot and lisp.pot into bookshops.pot
${PO_TEMPLATE}: ${PO_TEMPLATE_DIR}/djula.pot ${PO_TEMPLATE_DIR}/lisp.pot
	msgcat --use-first $^ > $@

# Rule to generate or update the .po files from the .pot file
locale/%/LC_MESSAGES/bookshops.po: ${PO_TEMPLATE}
	mkdir -p $(@D)
	[ -f $@ ] || msginit --locale=$* \
          -i $< \
          -o $@ \
	&& msgmerge --update $@ $<

# Rule to create the .mo files from the .po files
locale/%/LC_MESSAGES/bookshops.mo: locale/%/LC_MESSAGES/bookshops.po
	mkdir -p $(@D)
	msgfmt -o $@ $<
