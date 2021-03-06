# Style guide

The style guides of reference are:

- https://lisp-lang.org/style-guide/,
- itself following Google's: https://google.github.io/styleguide/lispguide.xml
- Chapter 29 of the SICL specification 'General Common Lisp style guide.
  http://metamodular.com/SICL/sicl-specification.pdf
- Use a 100 columns limit. There's a `.dir-locals.el` file for Emacs
  to automatically configure it correspondingly.

Below are rules to tackle issues we encountered in this project.

## Packages and symbols

- do not `:use` packages (unless there is no choice).
- prefer package-local-nicknames over `:import-from`.
