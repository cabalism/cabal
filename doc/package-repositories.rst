.. _package-repositories:

Package Repositories
====================

A package repository has two parts, the source archives of its packages and an
index of them. :ref:`Repositories are specified <repository-specification>` in
the configuration file or in a project.

.. _the-package-index:

The index
---------

A repository's index is its list of packages. It has the package description
of every version of every package, so that ``cabal`` can work out which
versions to use without downloading the packages themselves. ``cabal update``
downloads the index of each repository and keeps it in a subdirectory of
:cfg-field:`remote-repo-cache` named after the repository.

.. _the-index-log:

The log
-------

The index of a secure repository, such as Hackage, is a log. Each new package
version, each revision and each change to a package's preferred versions is
added to the end with the time it was added, as one of these entries:

.. code-block:: text

    PKG/VERSION/package.json
    PKG/VERSION/PKG.cabal
    PKG/preferred-versions

These are the entries Hackage's index has for ``megaparsec`` from one week of
June 2026: a new version, a revision of it, another new version and a change of
preferred versions.

.. code-block:: text

    2026-06-18T19:19:46Z megaparsec/9.8.0/package.json
    2026-06-18T19:19:46Z megaparsec/9.8.0/megaparsec.cabal
    2026-06-19T08:16:51Z megaparsec/9.8.0/megaparsec.cabal
    2026-06-23T16:03:40Z megaparsec/9.8.1/package.json
    2026-06-23T16:03:40Z megaparsec/9.8.1/megaparsec.cabal
    2026-06-23T16:04:47Z megaparsec/preferred-versions

A new version adds two entries, its package description and a ``package.json``
with the size and hashes of its source archive. A revision adds a package
description only. The source archives themselves are not in the index.

A later entry replaces an earlier one of the same name, so ``cabal`` uses the
last package description of each version and the last preferred versions of
each package. An :cfg-field:`index-state` cuts the log off at a time, and
``cabal`` sees nothing added after it. As the log only grows, ``cabal update``
downloads only what was added since the last update when it can.

.. _index-preferred-versions:

Preferred versions
------------------

A repository can prefer some versions of a package to others. A deprecated
version is one left out of the preferred range. The last entry above deprecates
``megaparsec-9.8.0``, along with a version that was deprecated already:

.. code-block:: text

    megaparsec <9.4.0 || >9.4.0 && <9.8.0 || >9.8.0

These are preferences, not constraints. The solver picks a version inside a
package's preferred range when it can. It can still pick a version outside the
range, for instance when a constraint requires that version.

Preferences are not tied to the repository they come from. When more than one
repository has preferred versions for a package, the ranges are combined as if
joined with ``&&``, and they apply to that package whichever repository it
comes from.

Index files
-----------

The file that holds the index depends on the kind of repository.

.. list-table::
    :header-rows: 1

    * - Kind of repository
      - Index file
    * - Secure, such as Hackage or one with ``secure: True``
      - ``01-index.tar``, a log
    * - Legacy, not secure
      - ``00-index.tar.gz``, not a log
    * - :ref:`No-index <no-index-layout>`, with a ``file+noindex:`` URL
      - None. ``cabal`` constructs the index from the files in the directory.

.. _no-index-layout:

No-index repositories
---------------------

A :ref:`no-index repository <local-no-index-repositories>` is a local
directory that can hold these files:

.. code-block:: text

    PATH/
        PKG-VERSION.tar.gz
        PKG-VERSION.cabal
        preferred-versions
        noindex.cache

Unrevised packages
^^^^^^^^^^^^^^^^^^

Each ``PKG-VERSION.tar.gz`` file is a package. On its own, ``cabal`` uses the
source and the ``.cabal`` file inside it.

.. code-block:: text

    PATH/
        foo-0.1.0.0.tar.gz

Revised packages
^^^^^^^^^^^^^^^^

A ``PKG-VERSION.cabal`` file beside a ``.tar.gz`` file of the same name revises
that package. ``cabal`` uses it in place of the ``.cabal`` file inside the
``.tar.gz`` file. Here ``bar-0.2.0.0`` uses the source inside
``bar-0.2.0.0.tar.gz`` and the package description in ``bar-0.2.0.0.cabal``.

.. code-block:: text

    PATH/
        bar-0.2.0.0.tar.gz
        bar-0.2.0.0.cabal

.. _no-index-cache:

The cache
^^^^^^^^^

``cabal`` writes the index it constructs to a ``noindex.cache`` file in the
directory, or elsewhere when the URL has a ``#shared-cache`` fragment.

.. note::
    ``cabal`` uses ``noindex.cache`` whenever it exists and does not check it
    against the directory. After changing the contents of the directory, run
    ``cabal update`` or delete ``noindex.cache`` so that the cache is rebuilt.

The preferred-versions file
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The optional ``preferred-versions`` file holds the repository's
:ref:`preferred versions <index-preferred-versions>`. Each line is a comment or
a package name followed by a version range. When more than one line names the
same package, their version ranges are combined as if joined with ``&&``.

.. code-block:: text

    -- Deprecate binary-0.8.0.0.
    binary < 0.8.0.0 || > 0.8.0.0
    -- Prefer text-1.2.0.0.
    text == 1.2.0.0

.. warning::
    A line that does not parse is skipped with a warning, and its preference is
    not applied. Each of these fails to parse:

    - a blank line
    - an indented comment, with spaces before the ``--``
    - a comment after the version range, as in ``text == 1.2.0.0 -- preferred``
