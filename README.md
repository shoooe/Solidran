###Solidran

This repository collects possibly ugly but *simple* and working solutions to Rosalind's problems. Within `Solidran/` The directory's name reflect the problem's ID on the [Rosalind list](http://rosalind.info/problems/list-view/).

Each solution directory contains a `README.md` file that briefly explains what is going on. For each solution a `dataset.txt` is also provided which is a real example of the dataset given from Rosalind.

####Building

To build, run:

```
cabal install --only-dependencies
cabal configure
cabal build
```
