===============
Company GraphQL
===============

GraphQL_ backend for company-mode_.

Introduction
------------

This emacs package provides completion for GraphQL file with your
GraphQL server, which host the GraphQL DSL and allow
introspection.

  .. image:: misc/demo5.gif

The type, fields, argument definition lookup process is
relied on the instrospection specified in `introspection.graphql`
graphql query file, which is used to get json response from the
server. Once the JSON response is received, it builds a hashtable,
which is futher augmented to help faster definition lookup.

Installation
------------

1. You can install this package from Melpa_

    M-x package-install RET company-graphql RET

2. add your `company-graphql` to `company-backend`

    (add-to-list 'company-backends 'company-graphql)

Usage
-----

1. open any `.graphql` file, and edit...

.. _GraphQL: https://github.com/graphql/
.. _company-mode: https://github.com/company-mode/company-mode/
.. _Melpa: http://melpa.milkbox.net/
