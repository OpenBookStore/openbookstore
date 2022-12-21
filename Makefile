LISP ?= sbcl

# List lisp files, unless they contains a #
SRC := $(shell find src/ -name '*.lisp' -a ! -name '*#*')

all: test

run:
	rlwrap $(LISP) --load run.lisp

build:
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

# list of supported locales
LOCALES := fr_FR # en_GB
# list of .po files
POS := $(foreach locale,$(LOCALES),locale/po/$(locale)/messages.po)
# list of .mo files
MOS := $(foreach locale,$(LOCALES),locale/mo/$(locale)/messages.mo)

.PHONY: tr
tr: ${MOS}

# Rule to extract translatable strings from SRC
locale/messages.pot: ${SRC}
	xgettext --keyword=_ --language=lisp -o locale/messages.pot $^

# Rule to generate or update the .po files from the .pot file
locale/po/%/messages.po: locale/messages.pot
	mkdir -p $(@D)
	[ -f $@ ] || msginit --locale=$* \
          -i $< \
          -o $@ \
	&& msgmerge --update $@ $<

# rule to create the .mo files from the .po files
locale/mo/%/messages.mo: locale/po/%/messages.po
	mkdir -p $(@D)
	msgfmt -o $@ $<
