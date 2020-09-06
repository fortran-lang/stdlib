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
