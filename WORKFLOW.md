# Workflow for the Fortran stdlib contributors

This document describes our current workflow. You are welcome to propose
changes in it by opening an
[issue](https://github.com/fortran-lang/stdlib/issues).

1. You have an idea or a proposal. Open an issue to discuss it. This is on the
   level of "is there interest in having image reader/writer functions in
   stdlib?"

2. When there seems to be significant interest in the proposal, like 80/20
   participants think it's a good/bad idea, move on to discuss the specific
   API. It's OK to propose the API off the bat if you already have an idea for
   it.

3. Discuss the API and iterate. When there is ~80/20 approval for the API, move
   on to implement it and submit a PR. Small PRs are always better than large.
   It's OK to implement only a few functions of a new module, and continue work
   on the others in a later PR. All new functionality goes into an
   "experimental" namespace (`stdlib_experimental_*.f90`).
   As part of the PR, when submitting a new public facing API, please provide
   the initial draft of the specification document as well as the the initial
   reference implementation of this specification.
   The specification is a document that describes the API and the
   functionality, so that anyone can use it to create an implementation from
   scratch without looking at `stdlib`. The `stdlib` library then provides the
   reference implementation.

4. When opening a PR, request reviews from one or more people that are most
   relevant to it. These are likely to be people involved in prior steps of the
   workflow. Other contributors (not explicitly invited) are encouraged to
   provide reviews and suggestions as well. Iterate until all (or most)
   participants are on the same page. We should not merge if there is a strong
   objection from the reviewers or from somebody in the wider community.

5. Moving from experimental to main. The experimental "namespace" contains new
   functionality together with its specification. In order to move from
   experimental to main, the specification document must be approved by the
   wide community and the standards committee (informally). If that happens,
   then we can move the code into main, and the particular specification
   document becomes part of the Fortran Standard Library.
