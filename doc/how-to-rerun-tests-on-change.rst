.. _how-to-rerun-tests-on-change:

How to rerun tests when files change
====================================

Cabal has no built-in watch mode (see `issue #5252
<https://github.com/haskell/cabal/issues/5252>`__), but two existing pieces
combine well to give one:

1. ``cabal repl`` can load a library and its test suite into a single GHCi
   session, so that a change to either is picked up by a plain ``:reload``.
2. `ghcid <https://github.com/ndmitchell/ghcid>`__ watches the files loaded
   in a GHCi session, reloads them when they change, and runs an expression
   of your choice after every successful reload.

Reloading in GHCi only recompiles the modules that changed and skips linking
and process start-up, so the feedback loop is usually much shorter than
rerunning ``cabal test``.

This guide uses a package ``mylib`` with a library and a test suite
``mylib-test``. Substitute your own package and component names.

Load the library and the test suite together
--------------------------------------------

A test suite depends on the library, so loading both into one session needs
GHCi's multiple home units support, which ``cabal repl`` exposes through the
:option:`--enable-multi-repl` flag of :ref:`cabal repl <cabal-repl>`.

.. note::

    GHC 9.4 to 9.12 can load several units and report errors on reload, but
    their GHCi cannot evaluate expressions from the loaded units or run
    ``:main``: every attempt fails with ``Not in scope`` or ``Command is not
    supported (yet) in multi-mode``. Running the test suite from a
    multi-unit session as described below needs GHC 9.14 or later. With an
    older GHC, use ghcid without ``--test`` to get fast error feedback, or
    rerun the compiled test suite as described at the end of this guide.

.. code-block:: console

    $ cabal repl --enable-multi-repl lib:mylib test:mylib-test

Check with ``:show modules`` that the modules of both components are listed
as loaded, and that they are loaded from source (``interpreted``), not from
a compiled package. If the library modules are missing, the test suite was
loaded against the installed library, and changes to the library will not be
seen by ``:reload``.

To avoid passing the flag every time, enable it in ``cabal.project``:

.. code-block:: cabal

    multi-repl: True

Run the test suite from the prompt
----------------------------------

In a multi-unit session no module is in scope at the prompt, so a bare
``:main`` reports that ``main`` is not in scope. Use the qualified name of
the test suite's entry point instead:

.. code-block:: none

    ghci> Main.main

A failing test suite typically calls ``exitFailure``, which GHCi reports as
``*** Exception: ExitFailure 1`` without leaving the session.

If ``Main.main`` is not in scope, check the GHC version (see the note above)
and ``:show modules``: the test suite's main module must be one of the
loaded modules and must be called ``Main``.

Watch and rerun with ghcid
--------------------------

Install ghcid with ``cabal install ghcid`` (or from your package manager),
then start it with the ``cabal repl`` command above and the expression to run
after each reload:

.. code-block:: console

    $ ghcid --command 'cabal repl --enable-multi-repl lib:mylib test:mylib-test' \
            --test Main.main --warnings

* ``--command`` is how ghcid starts GHCi. Anything ``cabal repl`` accepts can
  go here, for example ``-w ghc-9.10.3`` or ``--repl-options=-Wall``.
* ``--test`` is a GHCi expression to evaluate after every successful reload.
* ``--warnings`` runs the test even when the reload produced warnings; without
  it, ghcid only runs the test when the code is warning-free.

Save a file in either component and ghcid reloads the changed modules and
runs the test suite again. Compile errors are shown in place of the test
output until they are fixed.

To keep this invocation with the project, put the arguments in a ``.ghcid``
file at the project root, one per line, and start ghcid with no arguments:

.. code-block:: none

    --command=cabal repl --enable-multi-repl lib:mylib test:mylib-test
    --test=Main.main
    --warnings

Passing arguments to the test suite
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Test frameworks read their options from the program arguments, which GHCi
sets with ``:set args``. Pass that through ghcid's ``--setup`` flag, for
example to run a subset of a tasty or hspec suite:

.. code-block:: console

    $ ghcid --command 'cabal repl --enable-multi-repl lib:mylib test:mylib-test' \
            --setup ':set args --pattern Parser' --test Main.main --warnings

Speeding up the test run
^^^^^^^^^^^^^^^^^^^^^^^^

GHCi interprets the loaded modules by default. Reloading interpreted code is
fast, but running a large test suite through the interpreter can be slower
than running the compiled test executable. Ask GHCi to compile to object code
instead by passing the option to the REPL only, so that it does not affect
the configuration of other packages:

.. code-block:: console

    $ cabal repl --enable-multi-repl --repl-options=-fobject-code lib:mylib test:mylib-test

This trades a slower reload for a faster test run. Try both on your project.

Rerunning the compiled test suite instead
-----------------------------------------

If a test suite cannot run inside GHCi (for example because it depends on
being a separate process, or on flags that only apply to compiled code), use
a general-purpose file watcher to rerun ``cabal test`` when files change.
GHC's recompilation checker still only recompiles changed modules, but every
run pays for linking and for Cabal's own start-up:

.. code-block:: console

    $ watchexec --exts hs,cabal -- cabal test mylib-test

or, with `entr <https://github.com/eradman/entr>`__:

.. code-block:: console

    $ find src test -name '*.hs' | entr -c cabal test mylib-test

Other watchers
--------------

`ghciwatch <https://github.com/MercuryTechnologies/ghciwatch>`__ is an
alternative to ghcid that tracks which modules GHCi has loaded rather than
issuing a plain ``:reload``. At the time of writing it does not support
sessions with more than one component (see `ghciwatch issue #316
<https://github.com/MercuryTechnologies/ghciwatch/issues/316>`__), so it
cannot be used with ``--enable-multi-repl``. It works well for a single
component, for instance a test suite that lists the library's source
directory in its own ``hs-source-dirs``.
