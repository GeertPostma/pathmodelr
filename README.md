# Pathmodelr

Pathmodelr is an -in development- package for the R language.
It implements the Process PLS algorithm and provides a ton of nice helper functions to allow you to perform analysis as efficiently as possible!

## Installing the Pathmodelr package

### Installation using Devtools
The Pathmodelr package is still under development, and as such not yet released through CRAN.
To work with the Pathmodelr package (at your own risk!) you can follow the following instructions to set it up:

First, install the package using devtools: (install devtools first if you haven't already)

```
install.packages("devtools")

library(devtools)
```

Then the latest pseudo-stable version of the package can be installed directly using the following command:

```
devtools::install_github("Blackeel/pathmodelr", ref="0.12Release")
```
Alternatively, the `ref` can be changed to any Git branch to test a different release.

To get a rough grasp of the possibilities and to check if the installation was succesful, it is advisable to download and re-run the Process PLS experiments accompanying the main paper. These can be found at: https://github.com/Blackeel/processplssimulations/
