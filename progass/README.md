# Group 22's programming assignment
The solutions to each assignment is given in the respective folders.
To build each solution, it is preferable to use [the build system stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).
Then it is just a matter of going into the wanted folder and run `stack build` to build, or `stack run` to run the main module of that solution.

## Cryptolib
To run the test suite of the cyrptolibrary, one can invoke `stack test`.
The implementations of the cryptoprimitives reside in `cryptolib/src/CryptoLib/Primitives.hs`.

The cryptolibrary is registered as a library by cabal, and it is imported by other solutions as such. If one of the later solutions `(3, 4, 5)` fail to build, try to rebuild the cryptolibrary and then go and rebuild the solution. 

This is not optimal, but we also did not want to copy-paste the cryptolibrary everywhere.
