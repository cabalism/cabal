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

The example
-----------

This guide uses a package ``watched-pot`` with a library exposing the module
``Pot`` and a test suite ``never-boils``. It lives in Cabal's own test suite
at ``cabal-testsuite/PackageTests/MultiRepl/TestSuiteMain``, where CI checks
that the session set up below can run the test suite.

.. literalinclude:: ../cabal-testsuite/PackageTests/MultiRepl/TestSuiteMain/watched-pot.cabal
    :language: cabal
    :caption: watched-pot.cabal

.. literalinclude:: ../cabal-testsuite/PackageTests/MultiRepl/TestSuiteMain/src/Pot.hs
    :language: haskell
    :caption: src/Pot.hs

.. literalinclude:: ../cabal-testsuite/PackageTests/MultiRepl/TestSuiteMain/test/Main.hs
    :language: haskell
    :caption: test/Main.hs

Load the library and the test suite together
--------------------------------------------

A test suite depends on the library, so loading both into one session needs
GHCi's multiple home units support, available since GHC 9.4, which ``cabal
repl`` exposes through the :option:`--enable-multi-repl` flag of :ref:`cabal
repl <cabal-repl>`. List the test suite first:

.. code-block:: console

    $ cabal repl --enable-multi-repl test:never-boils lib:watched-pot

The order matters. Cabal makes the first target the *active unit* of the
session, and the ``Main`` module of a component can only be reached from the
prompt when that component is the active unit. Library modules such as
``Pot`` can be reached whichever unit is active.

Check with ``:show modules`` that the modules of both components are listed
and loaded from source (``interpreted``). If ``Pot`` is missing, the test
suite was loaded against the installed library, and changes to the library
will not be seen by ``:reload``.

To avoid passing the flag every time, enable it in ``cabal.project``:

.. code-block:: cabal

    multi-repl: True

Run the test suite from the prompt
----------------------------------

In a multi-unit session no module is in scope at the prompt, so on recent
GHCs a bare ``:main`` reports that ``main`` is not in scope. The qualified
name of the test suite's entry point works on every GHC since 9.4:

.. code-block:: none

    ghci> Main.main
    Watching the pot...
    The watched pot never boils early.

A failing test suite typically calls ``exitFailure``, which GHCi reports as
``*** Exception: ExitFailure 1`` without leaving the session. Try it: change
``100`` to ``101`` in ``src/Pot.hs``, ``:reload``, and run ``Main.main``
again.

If ``Main.main`` is not in scope, the test suite was not the first target.
Check the order of the targets and ``:show modules``: the test suite's main
module must be one of the loaded modules and must be called ``Main``.

Watch and rerun with ghcid
--------------------------

Install ghcid with ``cabal install ghcid`` (or from your package manager),
then start it with the ``cabal repl`` command above and the expression to run
after each reload:

.. code-block:: console

    $ ghcid --command 'cabal repl --enable-multi-repl test:never-boils lib:watched-pot' \
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

    --command=cabal repl --enable-multi-repl test:never-boils lib:watched-pot
    --test=Main.main
    --warnings

Passing arguments to the test suite
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Test frameworks read their options from the program arguments, which GHCi
sets with ``:set args``. Pass that through ghcid's ``--setup`` flag, for
example to run a subset of a tasty or hspec suite:

.. code-block:: console

    $ ghcid --command 'cabal repl --enable-multi-repl test:never-boils lib:watched-pot' \
            --setup ':set args --pattern boils' --test Main.main --warnings

Speeding up the test run
^^^^^^^^^^^^^^^^^^^^^^^^

GHCi interprets the loaded modules by default. Reloading interpreted code is
fast, but running a large test suite through the interpreter can be slower
than running the compiled test executable. Ask GHCi to compile to object code
instead by passing the option to the REPL only, so that it does not affect
the configuration of other packages:

.. code-block:: console

    $ cabal repl --enable-multi-repl --repl-options=-fobject-code test:never-boils lib:watched-pot

This trades a slower reload for a faster test run. Try both on your project.

Rerunning the compiled test suite instead
-----------------------------------------

If a test suite cannot run inside GHCi (for example because it depends on
being a separate process, or on flags that only apply to compiled code), use
a general-purpose file watcher to rerun ``cabal test`` when files change.
GHC's recompilation checker still only recompiles changed modules, but every
run pays for linking and for Cabal's own start-up:

.. code-block:: console

    $ watchexec --exts hs,cabal -- cabal test never-boils

or, with `entr <https://github.com/eradman/entr>`__:

.. code-block:: console

    $ find src test -name '*.hs' | entr -c cabal test never-boils

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
