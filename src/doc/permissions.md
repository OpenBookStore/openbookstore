# How the permission system works

## General

* There are users and roles, roles determine access permission.
* Roles just have a name (their primary key), and e.g. `add-role` just takes a corresponding keyword.
* Roles build a hierarchy (a tree, see `define-role`, although the existing roles build a linear chain): admin > editor > vendor > visitor.

See also:

1. [issue#3: Request: user roles and rights, admin login](https://gitlab.com/myopenbookstore/openbookstore/-/issues/3)
1. [issue#22: Discuss: how to better use roles in templates](https://gitlab.com/myopenbookstore/openbookstore/-/issues/22)

## Code overview

(File paths are from within `src/`.)

* `authentication.lisp`: defines the `user`, `role`, `user-role`,
  `role-copy` classes, and associated actions, in package
  `bookshops.models`):

    * `create-user`, `create-superuser`, `create-role`, `login` (just
      returns the user object if password is correct), `get-user`,
      `search-user` (find user by name or mail), `is-superuser`,
      `list-admin-users`, `remove-user`: procedures
    * `add-role`, `inherit-role`, `can:user-roles`: generic
    * `define-role` macro `(name (&rest inherits))`: used in `bootstrap-base-roles` (`database.lisp`)
    * `define-role-access` macro: used in `web/web.lisp` to give routes limited access

* `web/authentication.lisp`:

    * holds `current-user` via `hunchentoot:session-value`
    * defines routes via `defroute`
    * defines procedures called from routes:
        * `web-login`: used in `post-login-route`
        * `logout`: used in `post-logout-route`
    * defines `login-redirect`: used in `@check-roles`
    * defines `@check-roles`: used as a [easy-routes](https://github.com/mmontone/easy-routes) decorator in route definitions (see `web/web.lisp`)
    * defines `render-template*` macro: used in `web/web.lisp` and `web/authentication.lisp`

* `../tests/test-authentication.lisp`: tests, obviously, are more needed?
