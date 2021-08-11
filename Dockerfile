#XXX: Experimental, needs more work.
# Currently fails when trying to open the db.db (lacking?).

FROM clfoundation/sbcl:latest
COPY . ~/projets/openbookstore/openbookstore/
WORKDIR ~/projets/openbookstore/openbookstore/

RUN DEBIAN_FRONTEND=noninteractive apt-get update && \
        apt-get install -y \
        sqlite3
RUN QUICKLISP_ADD_TO_INIT_FILE=true /usr/local/bin/install-quicklisp
#TODO: install Quicklisp dependencies here, not at every start up.

CMD [ "sbcl", "--load", "./run.lisp" ]
