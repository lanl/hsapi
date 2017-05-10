HSAPI
====
These are (incomplete) Haskell bindings to the SAPI interface for DWave
machines. In general, the names are transformed from the C API by removing
`sapi` prefix and changing from underscore to camel case. 

Usage
-----
There is a simple `SAPI` monad for more composable error checking.
See `examples/simple.hs` for simple example usage.

Installation
------------
`cabal install`

Acknowledgements
----------------
This package is part of the Hybrid Quantum-Classical Computing suite, known
internally as LA-CC-16-032.
