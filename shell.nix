{ pkgs ? import <nixpkgs> {} }:

with pkgs;
let
  libraries = [
    openssl
    sqlite
    readline
  ];

  lispWithPackages = pkgs.lispPackages_new.lispWithPackages;

  sbcl = (lispWithPackages "${pkgs.sbcl}/bin/sbcl --script"
    (p: [
      p.asdf
      p.alexandria
      p.cacle
      p.can
      p.cl-ansi-text
      p.cl-i18n
      p.cl-json
      p.cl-ppcre
      p.cl-slug
      p.clss
      p.dbd-sqlite3
      p.dexador
      p.djula
      p.easy-routes
      p.function-cache
      p.hunchentoot
      p.local-time
      p.local-time-duration
      p.log4cl
      p.lquery
      p.mito
      p.mito-auth
      p.parse-float
      p.parse-number
      p.plump
      p.replic
      p.serapeum
      p.str
      p.trivial-backtrace
      p.unix-opts

      p.deploy
      p.fiveam

      p.swank
      p.slynk
    ]));

  deps = stdenv.mkDerivation {
    name = "bookshops-deps";

    src = ./bookshops.asd;

    phases = [ "installPhase" ];
    installPhase = ''
      echo Dependencies: >> $out
      ${sbcl}/bin/sbcl \
        --noinform \
        --non-interactive \
        --eval '(require :asdf)' \
        --eval '(asdf:load-asd "'$src'")' \
        --eval '(format t "~{ - ~a~^~&~}" (asdf:system-depends-on (asdf:find-system "bookshops")))' \
        | sort >> $out
    '';
  };

in mkShell {
  buildInputs = with pkgs; [
    rlwrap
    sbcl
    gnumake
    gettext
  ] ++ libraries;
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath(libraries)}
    # fstamour: I run it on a different port because 4242 is
    # already used on my computer
    export OBS_PORT=4343
    # uncomment to get a list of dependencies without building
    # the whole application
    # cat ${deps}
  '';
}
