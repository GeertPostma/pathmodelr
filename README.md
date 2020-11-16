# Pathmodelr

Pathmodelr is an -in development- package for the R language.
It implements the Process PLS algorithm and provides a ton of nice helper functions to allow you to perform analysis as efficiently as possible!

## Installing the Pathmodelr package
The Pathmodelr package is still under development, and as such not yet released through CRAN.
To work with the Pathmodelr package (at your own risk!) you can follow the following instructions to set it up.
We provide installation instructions for our pre-built package as well as directly from the Github repo using devtools.

### Installation from pre-built package
First, download [the latest release from Github](https://github.com/Blackeel/pathmodelr/releases/tag/v0.12) and save it to your disk.

The dependencies will then need to be installed. These dependencies are: 'caret', 'dplyr', 'ggplot2', 'listenv', 'reshape2', 'R6', 'network', 'ggnetwork', 'diagram'

You can install them by running `install_packages('package_name')` for each package manually, or by using the following command:
```
install.packages(c('caret', 'dplyr', 'ggplot2', 'listenv', 'reshape2', 'R6', 'network', 'ggnetwork', 'diagram'))
```


After the dependencies are isntalled you can install the package using the following command:
```
install.packages("D:\\Repos\\pathmodelr_0.12.tar.gz", repos= NULL, type="source")
```


### Installation using Devtools
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


## Testing Pathmodelr
To get a rough grasp of the possibilities and to check if the installation was succesful, it is advisable to download and re-run the Process PLS experiments accompanying the main paper. These can be found at: https://github.com/Blackeel/processplssimulations/
