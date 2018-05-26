### Research compendium for "srctrust" by Magnus Hoem Iversen, Mikael Poul Johannesson, and Erik Knudsen.

[![Travis-CI Build Status](https://travis-ci.org/mikajoh/srctrust.svg?branch=master)](https://travis-ci.org/mikajoh/srctrust)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mikajoh/srctrust?branch=master&svg=true)](https://ci.appveyor.com/project/mikajoh/srctrust)
[![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/mikajoh/srctrust/master/LICENSE)

This work in progress.

Comments, questions, and suggestions are very welcomed! Please contact me at [mikael.johannesson@uib.no](mailto:mikael.johannesson@uib.no).

#### Install

You can install the reasearch compendium by running (note that you need the `devtools` package installed):

```r
library(devtools)
install_github("mikajoh/srctrust")
```

#### Includes

The compendium included several functions used in the analysis (see for instance`?srctrust::amce`).

In addition, it includes:

- `analysis/`: R-code needed to reproduce the results
  -  `analysis/01_data.R`: Prepares the raw data the NCP and outputs `trust.csv`.
- `analysis/data/`: The prepared data (`trust.csv`).*

----

*Note: the raw data is freely available for researchers via [NSD](http://www.nsd.uib.no/nsddata/serier/norsk_medborgerpanel_eng.html)
