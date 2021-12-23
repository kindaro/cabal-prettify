# `cabal-prettify`: Prettify your Cabal package configuration files!

## Quick start:

* Prettify the configuration file of the package you are in right now:

      cabal-prettify --this

* Prettify specified configuration files:

      cabal-prettify cabal-prettify.cabal examples/**/*.cabal

* Prettify standard input:

      cabal-prettify --filter < cabal-prettify.cabal

## Minutiae:

1. Prettified files are backed up with the extension `*.previous`.

   1. Files that are already pretty are not backed up.
   
      This ensures that running `cabal-prettify` twice in a row does not overwrite the original.

## Prior art:

* [`cabal-fmt`]

[`cabal-fmt`]: https://github.com/phadej/cabal-fmt
