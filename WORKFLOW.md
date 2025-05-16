# Workflow for the Fortran stdlib contributors

This document describes our current workflow.

We welcome everyone and anyone to participate and propose additions to stdlib.
It is okay if you do not have experience for specification or implementation,
but have an idea for stdlib. If the idea is popular among the community, more
experienced contributors will help it through all 5 steps.


1. **Idea**: You have an idea or a proposal. Open an
   [issue](https://github.com/fortran-lang/stdlib/issues) to discuss it. This
   is on the level of "is there interest in having image reader/writer
   functions in stdlib?" The goal of this step is to find out if the community
   is interested in having this functionality as part of stdlib.

2. **API**: When there seems to be significant interest in the proposal (vast
   majority of participants think it is a good idea), move on to discuss the
   specific API. It's OK to propose the API off the bat if you already have an
   idea for it. This step is exploratory and its goal is to find out what the
   API should *look* and *feel* like.

3. **Specification**: Discuss the API and iterate. When there is vast majority
   approval for the API, move on to implement it and submit a PR. Small PRs are
   always better than large.  It is OK to implement only a few functions of a
   new module, and continue work on the others in a later PR. All new
   functionality goes into an "experimental" namespace
   (`version: experimental`). As part of the PR, when submitting a new
   public facing API, please provide the initial draft of the specification
   document as well as the initial reference implementation of this
   specification.  The
   [specification is a document](https://stdlib.fortran-lang.org/page/specs/index.html)
   that describes the API and
   the functionality, so that anyone can use it to create an implementation
   from scratch without looking at `stdlib`. The `stdlib` library then provides
   the reference implementation.

4. **Implementation** in experimental: When opening a PR, request reviews from
   one or more people that are most relevant to it. These are likely to be
   people involved in prior steps of the workflow. Other contributors (not
   explicitly invited) are encouraged to provide reviews and suggestions as
   well. Iterate until all (or most) participants are on the same page.
   A merge is permitted if there are unit tests for a majority of the possible
   calling scenarios (with or without optional arguments, with arguments that
   trigger an error) and if there is vast majority approval of the PR.

5. **Release**: Moving from experimental to release. The experimental
   "namespace" contains new functionality together with its specification. In
   order to move from experimental to release, the specification document must
   be approved by the wide community and the standards committee (informally).
   If that happens, it has now been blessed for broad use and we can move the
   code into the main section of `stdlib`, and the particular specification
   document becomes part of the Fortran Standard Library.


Note: the general term "vast majority" above means at least 80%, but ultimately
it is left to our best judgement to ensure that the community agrees that each
PR and proposal was approved by "vast majority".

You are welcome to propose changes to this workflow by opening an
[issue](https://github.com/fortran-lang/stdlib/issues).


## Build systems

This project supports two build systems,
[fpm](https://github.com/fortran-lang/fpm) and CMake.

### CMake build files

The build files for CMake allow both in-tree, *i.e.* build artifacts share
the same tree as the source files, and out-of-tree builds, *i.e.* build artifacts
exist in a separate directory tree.
Both build types are explicitly supported and tested, the latter strategy
is recommended for local development.

Sources for the main library target are added in ``src/CMakeLists.txt``
relative to the library target, *i.e.* no absolute paths are required.

To add tests, the macro ``ADDTEST`` should be used instead of the CMake function
``add_test``, the macro hides creation of the executable target, linking against the 
main library target and registering the test.
The tests themselves are defined as standalone executables in the subdirectories
in ``test``, a new subdirectory with tests has to be registered in
``test/CMakeLists.txt``.

The source tree should be considered read-only. References to ``PROJECT_SOURCE_DIR``
and ``CMAKE_CURRENT_SOURCE_DIR`` should only be used for accessing source files,
never to write build outputs, use ``PROJECT_BINARY_DIR`` and ``CMAKE_CURRENT_BINARY_DIR``
to write build artifacts instead.
To fully support in-tree builds, build artifacts must never have the same name as
source files to avoid accidentally overwriting them, *e.g.* when preprocessing or
configuring a file.

The ``CMAKE_INSTALL_PREFIX`` should only be written to on install, never in the build
process. To install generated files, create a build output in the build tree and
install it with the ``install`` function.
This project follows the GNU install conventions, this means that the variables
``CMAKE_INSTALL_BINDIR``, ``CMAKE_INSTALL_LIBDIR``, and ``CMAKE_INSTALL_INCLUDEDIR``
must be used instead of ``bin``, ``lib``, and ``include``, respectively.
Library targets should be exported on install to allow correct inclusion of the
project in other CMake projects.
Prefer dashes as in ``project-config`` or ``project-targets`` over camel-case as in
``projectConfig`` or ``projectTarget`` for file names as the former allows easier
construction from the ``PROJECT_NAME`` variable by concatenation.

The project is usable as CMake subproject. Explicit references to
``CMAKE_SOURCE_DIR`` and ``CMAKE_BINARY_DIR`` must be avoided to not
break subproject builds.
An example project is available [here](https://github.com/fortran-lang/stdlib-cmake-example)
to test the CMake subproject integration.

## GitHub collaboration

Contributing can be daunting, we know! Even more for a big project with many contributors, and if you are not expert on the whole github workflow then even more, we have been there at some point.

In order to help lowering the barrier for collaborating on ongoing efforts (e.g. an open PR), we have crafted a simple script that might come in handy. To explain the process we'll use Alice (the person you want to help) and Bob (you):

       ┌────────────────────────────┐
       │    fortran-lang/stdlib     │
       └────────────▲───────────────┘
                    │
                    │  [Pull Request]
                    │
            ┌───────┴────────┐
            │  alice/stdlib  │ ←─────┐
            └──────▲─────────┘       │
                   │                 │
   [PR Branch] ←───┘          ┌──────┴──────┐
   feature-branch             │  bob/stdlib │
   (hosted here)              │    (fork)   │
                              └─────────────┘
                                     ▲
                                     │
                       [Push access to Alice's repo]

After having forked from `fortran-lang/stdlib` and cloned your `stdlib` fork on your local machine; on an unix compatible terminal with access to the `git` CLI, being at the root folder: 
```sh
./.github/collab.sh
```
You will be asked to enter the username and branch of the other person:
```bash
Enter the GitHub username of the fork owner (e.g., alice): alice
Enter the PR branch name (e.g., feature-branch): feature-branch
```
This will fetch Alice's repository and switch your view to Alice's feature-branch. Now you can review, build, run, play around, propose your nice improvements.

Once you finish helping out, you can always `git checkout <your_own_branch>` and/or delet Alice's branch from your local view `git branch -d feature-branch`.

Remember, announce your willingness to help 😉
