Configuration
=============

.. highlight:: cabal

Overview
--------

The global configuration file for ``cabal-install`` is by default
``$XDG_CONFIG_HOME/cabal/config``. If you do not have this file, ``cabal`` will create
it for you on the first call to ``cabal update``
(details see `configuration file discovery`_).
Alternatively, you can explicitly ask ``cabal`` to create it for you using

.. code-block:: console

    $ cabal user-config update

You can change the location of the global configuration file by specifying
either ``--config-file=FILE`` on the command line or by setting the
``CABAL_CONFIG`` or ``CABAL_DIR`` environment variable.

Most of the options in this configuration file are also available as
command line arguments, and the corresponding documentation can be used
to lookup their meaning. The created configuration file only specifies
values for a handful of options. Most options are left at their default
value, which it documents; for instance,

::

    -- executable-stripping: True

means that the configuration file currently does not specify a value for
the ``executable-stripping`` option (the line is commented out), and
that the default is ``True``; if you wanted to disable stripping of
executables by default, you would change this line to

::

    executable-stripping: False

You can also use ``cabal user-config update`` to migrate configuration
files created by older versions of ``cabal``.

Environment variables
---------------------

Various environment variables affect ``cabal-install``.

``CABAL_CONFIG``
   The variable to find global configuration file.

``CABAL_DIR``

   If set, *all* ``cabal-install`` content files will be stored as
   subdirectories of this directory, including the configuration file
   if ``CABAL_CONFIG`` is unset.  If ``CABAL_DIR`` is unset, Cabal
   will store data files according to the XDG Base Directory
   Specification (see `directories`_).

   .. note::

       For backwards compatibility, if the directory ``~/.cabal`` on
       Unix or ``%APPDATA%\cabal`` on Windows exists, and
       ``$XDG_CONFIG_HOME/cabal/config`` does not exist, and
       ``CABAL_DIR`` is unset, ``cabal-install`` will behave as if
       ``CABAL_DIR`` was set to point at this directory.

``CABAL_BUILDDIR``

    The override for default ``dist`` build directory.
    Note, the nix-style builds build directory (``dist-newstyle``)
    is not affected by this environment variable.

.. _config-file-discovery:

Configuration file discovery
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The configuration file location is determined as follows:

1. If option ``--config-file`` is given, use it;
2. otherwise, if ``$CABAL_CONFIG`` is set use it;
3. otherwise, if ``$CABAL_DIR`` is set use ``$CABAL_DIR/config``;
4. otherwise, if ``~/.cabal`` exists, and is a directory, use
   ``~/.cabal/config``;
5. otherwise use ``config`` in ``$XDG_CONFIG_HOME/cabal``, which
   defaults to ``~/.config/cabal`` on Unix.

If the configuration file does not exist, and it was not given
explicitly via ``--config-file`` or ``$CABAL_CONFIG``, then
``cabal-install`` will generate the default one, with directories
based on ``$CABAL_DIR`` (if set) or according to the XDG Base
Directory Specification, as listed below.

.. _directories:

Directories
-----------

Unless the ``CABAL_DIR`` environment variable is set or a ``~/.cabal``
directory exists, Cabal will by default store data in directories
according to the XDG Base Directory Specification.  The following
directories are used unless otherwise specified in the configuration
file:

* ``$XDG_CONFIG_HOME/cabal`` for the main configuration file.
  Defaults to ``~/.config/cabal`` on Unix, and ``%APPDATA%/cabal`` on
  Windows.  Overridden by the ``CABAL_CONFIG`` environment variable if
  set.

* ``$XDG_CACHE_HOME/cabal`` for downloaded packages and script
  executables.  Defaults to ``~/.cache/cabal`` on Unix, and
  ``%LOCALAPPDATA%/cabal`` on Windows.  You can delete this directory
  and expect that its contents will be reconstructed as needed.

* ``$XDG_STATE_HOME/cabal`` for compiled libraries and other stateful
  artifacts, including the Cabal store.  Defaults to
  ``~/.local/state/cabal`` on Unix and ``%LOCALAPPDATA%/cabal`` on
  Windows.  Deleting this directory might cause installed programs to
  stop working.

* ``~/.local/bin`` for executables installed with ``cabal install``.

You can run ``cabal path`` to see a list of the directories that
``cabal`` will use with the active configuration.

.. _repository-specification:

Repository specification
------------------------

An important part of the configuration is the specification of the
repository. When ``cabal`` creates a default config file, it configures
the repository to be the central Hackage server:

::

    repository hackage.haskell.org
      url: http://hackage.haskell.org/

The name of the repository is given on the first line, and can be
anything; packages downloaded from this repository will be cached under
``$XDG_CACHE_HOME/cabal/packages/hackage.haskell.org`` (or whatever name you specify;
you can change the prefix by changing the value of
:cfg-field:`remote-repo-cache`). If you want, you can configure multiple
repositories, and ``cabal`` will combine them and be able to download
packages from any of them.

Using secure repositories
^^^^^^^^^^^^^^^^^^^^^^^^^

When interacting with ``hackage.haskell.org``, Cabal always runs in secure mode
with standard root keys, so it is not necessary to specify ``secure`` or
``root-keys``. If no repositories are listed, Cabal will default to
``hackage.haskell.org``.

For non-Hackage repositories that support the TUF security infrastructure you
can enable secure access to the repository by specifying:

::

    repository packages.example.org
      url: http://packages.example.org/
      secure: True
      root-keys: <root-key-IDs>
      key-threshold: <key-threshold>

The ``<root-key-IDs>`` and ``<key-threshold>`` values are used for
bootstrapping. As part of the TUF infrastructure the repository will
contain a file ``root.json`` (for instance,
http://hackage.haskell.org/root.json) which the client needs to do
verification. However, how can ``cabal`` verify the ``root.json`` file
*itself*? This is known as bootstrapping: if you specify a list of root
key IDs and a corresponding threshold, ``cabal`` will verify that the
downloaded ``root.json`` file has been signed with at least
``<key-threshold>`` keys from your set of ``<root-key-IDs>``.

You can, but are not recommended to, omit these two fields. In that case
``cabal`` will download the ``root.json`` field and use it without
verification. Although this bootstrapping step is then unsafe, all
subsequent access is secure (provided that the downloaded ``root.json``
was not tampered with). Of course, adding ``root-keys`` and
``key-threshold`` to your repository specification only shifts the
problem, because now you somehow need to make sure that the key IDs you
received were the right ones. How that is done is however outside the
scope of ``cabal`` proper.

More information about the security infrastructure can be found at
https://github.com/haskell/hackage-security.

Local no-index repositories
^^^^^^^^^^^^^^^^^^^^^^^^^^^

A directory of ``.tar.gz`` package files can be used as a local package
repository. ``cabal`` constructs the index for it.

::

    repository my-local-repository
      url: file+noindex:///absolute/path/to/directory

On Windows systems, the URL must start directly with the absolute path, as in
``url: file+noindex:C:/absolute/path/to/directory``. The scheme has to be
``file+noindex:``. A URL with the scheme ``file:`` is interpreted as a remote
repository, as described in the previous sections, and needs an
``01-index.tar`` file that you construct yourself.

The directory can hold these files:

.. code-block:: text

    /absolute/path/to/directory/
        foo-0.1.0.0.tar.gz
        bar-0.2.0.0.tar.gz
        bar-0.2.0.0.cabal
        preferred-versions
        noindex.cache

``package-name-version.tar.gz``
    A package. Here ``foo-0.1.0.0`` uses the source and the ``.cabal`` file
    inside ``foo-0.1.0.0.tar.gz``.

``package-name-version.cabal``
    Optional. A package description that is used in place of the one inside
    the ``.tar.gz`` file of the same name, as a new revision. Here
    ``bar-0.2.0.0`` uses the source inside ``bar-0.2.0.0.tar.gz`` and
    ``bar-0.2.0.0.cabal``.

``preferred-versions``
    Optional. Version preferences, described below.

``noindex.cache``
    Written by ``cabal``. The cache of the index it constructs from the other
    files.

If the directory is not writable, append a ``#shared-cache`` fragment to the
URL. The cache is then stored inside the :cfg-field:`remote-repo-cache`
directory, in a subdirectory named after the repository and a hash of its path.

.. note::
    ``cabal`` uses ``noindex.cache`` whenever it exists and does not check it
    against the directory. After changing the contents of the directory, run
    ``cabal update`` or delete ``noindex.cache`` so that the cache is rebuilt.

A local no-index repository is a directory of files, not a log of timestamped
entries, so there is no earlier state for an :cfg-field:`index-state` to
select. ``cabal`` always uses every package in the directory. An
``index-state`` that is unscoped, or scoped to this repository, is ignored for
it with a warning.

The ``preferred-versions`` file prefers or deprecates versions of packages.
These are preferences, not constraints. The solver picks a version inside a
package's range when it can. It can still pick a version outside the range, for
instance when a constraint requires that version.

The preferences also apply to packages from other repositories. A local
no-index repository with only a ``preferred-versions`` file in it can be used
to prefer or deprecate versions of packages from Hackage. Add the repository to
your :ref:`configuration file <config-file-discovery>` to have the preferences
apply to every project.

Each line of ``preferred-versions`` is a comment or a package name followed by
a version range. When more than one line names the same package, their version
ranges are combined as if joined with ``&&``.

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

Legacy repositories
^^^^^^^^^^^^^^^^^^^

Currently ``cabal`` supports single kind of “legacy” repositories.
It is specified using

::

    remote-repo: hackage.haskell.org:http://hackage.haskell.org/packages/archive

This is just syntactic sugar for

::

    repository hackage.haskell.org
      url: http://hackage.haskell.org/packages/archive

although, in (and only in) the specific case of Hackage, the URL
``http://hackage.haskell.org/packages/archive`` will be silently
translated to ``http://hackage.haskell.org/``.

Secure local repositories
^^^^^^^^^^^^^^^^^^^^^^^^^

If you want to use repositories on your local file system, it is
recommended instead to use a *secure* local repository:

::

    repository my-local-repo
      url: file:/path/to/local/repo
      secure: True
      root-keys: <root-key-IDs>
      key-threshold: <key-threshold>

The layout of these secure local repos matches the layout of remote
repositories exactly; the :hackage-pkg:`hackage-repo-tool`
can be used to create and manage such repositories.

.. _program_options:

Program options
---------------

Programs that ``cabal`` knows about can be provided with options that will be
passed in whenever the program is invoked by ``cabal``. The configuration file
can contain a stanza of ``program-default-options`` with ``<prog>-options``
fields to specify these.

::

  program-default-options
    ghc-options: ...
    happy-options: ...

The list of known programs is:

+-----------------------+------------------------------------------------------------------------------------------------------------------------------------+
| Program               | Notes                                                                                                                              |
+=======================+====================================================================================================================================+
| ``alex``              | `Alex User Guide <https://haskell-alex.readthedocs.io/en/latest/>`_                                                                |
+-----------------------+------------------------------------------------------------------------------------------------------------------------------------+
| ``ar``                | Usually provided by GHC's ``"ar command"`` entry in ``ghc --info``. Note this might refer to ``llvm-ar`` instead of GNU's ``ar``.  |
+-----------------------+------------------------------------------------------------------------------------------------------------------------------------+
| ``c2hs``              | :hackage-pkg:`c2hs on Hackage <c2hs>`                                                                                              |
+-----------------------+------------------------------------------------------------------------------------------------------------------------------------+
| ``cpphs``             | :hackage-pkg:`cpphs on Hackage <cpphs>`                                                                                            |
+-----------------------+------------------------------------------------------------------------------------------------------------------------------------+
| ``doctest``           | :hackage-pkg:`doctest on Hackage <doctest>`                                                                                        |
+-----------------------+------------------------------------------------------------------------------------------------------------------------------------+
| ``gcc``               | Usually provided by GHC's ``"C compiler command"`` entry in ``ghc --info``. Note this might refer to ``clang`` instead of ``gcc``. |
+-----------------------+------------------------------------------------------------------------------------------------------------------------------------+
| ``ghc``               | `GHC User's Guide <https://downloads.haskell.org/ghc/latest/docs/users_guide/>`_                                                   |
+-----------------------+------------------------------------------------------------------------------------------------------------------------------------+
| ``ghc-pkg``           | `GHC User's Guide: Package management                                                                                              |
|                       | <https://downloads.haskell.org/ghc/latest/docs/users_guide/packages.html#package-management-the-ghc-pkg-command>`_                 |
+-----------------------+------------------------------------------------------------------------------------------------------------------------------------+
| ``haddock``           | `Haddock User Guide <https://haskell-haddock.readthedocs.io/latest/>`_                                                             |
+-----------------------+------------------------------------------------------------------------------------------------------------------------------------+
| ``happy``             | `Happy User Guide <https://haskell-happy.readthedocs.io/en/latest/>`_                                                              |
+-----------------------+------------------------------------------------------------------------------------------------------------------------------------+
| ``hpc``               | :hackage-pkg:`hpc on Hackage <hpc>`                                                                                                |
+-----------------------+------------------------------------------------------------------------------------------------------------------------------------+
| ``hsc2hs``            | :hackage-pkg:`hsc2hs on Hackage <hsc2hs>`                                                                                          |
+-----------------------+------------------------------------------------------------------------------------------------------------------------------------+
| ``hscolour``          | :hackage-pkg:`hscolour on Hackage <hscolour>`                                                                                      |
+-----------------------+------------------------------------------------------------------------------------------------------------------------------------+
| ``jhc``               | `jhc homepage <http://repetae.net/computer/jhc/>`_                                                                                 |
+-----------------------+------------------------------------------------------------------------------------------------------------------------------------+
| ``ld``                | Usually provided by GHC's ``"ld command"`` entry in ``ghc --info``.                                                                |
+-----------------------+------------------------------------------------------------------------------------------------------------------------------------+
| ``pkg-config``        | Used to find the dependencies listed in :pkg-field:`pkgconfig-depends`, see the                                                    |
|                       | `pkg-config homepage <https://www.freedesktop.org/wiki/Software/pkg-config/>`_.                                                    |
+-----------------------+------------------------------------------------------------------------------------------------------------------------------------+
| ``runghc``            | `GHC User's Guide: Using runghc <https://downloads.haskell.org/ghc/latest/docs/users_guide/runghc.html>`_                          |
+-----------------------+------------------------------------------------------------------------------------------------------------------------------------+
| ``strip``             | Used when installing if :cfg-field:`executable-stripping` or :cfg-field:`library-stripping` is enabled, see                        |
|                       | `GNU strip <https://sourceware.org/binutils/docs/binutils/strip.html>`_.                                                           |
+-----------------------+------------------------------------------------------------------------------------------------------------------------------------+
| ``tar``               | Used by :ref:`setup-sdist` to create the source tarball, see `GNU tar <https://www.gnu.org/software/tar/>`_.                       |
+-----------------------+------------------------------------------------------------------------------------------------------------------------------------+
| ``uhc``               | `UHC on GitHub <https://github.com/UU-ComputerScience/uhc>`_                                                                       |
+-----------------------+------------------------------------------------------------------------------------------------------------------------------------+

.. warning::

  It is important to not confuse these options with the ones listed in the
  :ref:`build info<build-info>` section. The ``*-options`` fields mentioned are in
  fact syntactic sugar for specific ``ghc-options`` that will be passed only on
  certain phases.

.. warning::

  These options will be used when ``cabal`` invokes the tool as part of the build process or as part of a
  :pkg-field:`build-tool-depends` declaration, not whenever the tool is invoked by
  third parties.

  In particular this means that for example ``gcc-options`` will be used when ``cabal``
  invokes ``gcc``, which is **not** when C sources are compiled by GHC (even though GHC
  might invoke ``gcc`` internally). In order to provide options through GHC for those programs, one has to check the
  GHC User guide's `Section <https://downloads.haskell.org/ghc/latest/docs/users_guide/phases.html#forcing-options-to-a-particular-phase>`_.
  In short, those options have to be given as ``-opt<phase>`` flags to GHC.

.. note::

  The only case that violates the rule specified in this last warning above is
  ``ld-options``, which get passed as ``-optl`` options when GHC is invoked for
  linking, as with the :pkg-field:`ld-options` field in package descriptions.
  Notably, although ``gcc-options`` could be passed as :pkg-field:`cc-options`
  in the appropriate phases, they are actually **not** passed.
