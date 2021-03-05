
# labzenr

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

`labzen` is a R package that adds more [zen](https://en.wikipedia.org/wiki/Zen) to your student experience of working on [MDS](https://masterdatascience.ubc.ca/) labs. It lets you manage common tasks such as counting total marks in an assigment, and performs common checks for mechanics in your iPython notebooks and R markdown assignments.

## Features

**labzen** helps members of the [UBC Master of Data Science (MDS)](https://masterdatascience.ubc.ca/) manage lab assignments written in iPython notebooks and R markdown. The package saves precious student time by automating common tasks such as counting up total marks in a lab assignment, and performing common mechanical checks that can-- if overlooked-- lose a student easy marks.

The package is currently under development, but will include the following functions:

- **Function 1**: The internal `parse_lab()` function will take an MDS .ipynb or .Rmd lab and return its markdown contents as a list/vector of strings. The function will scrub out yaml, code blocks, and all other metadata.

- **Function 2**: `count_points()` will build upon the first function and further parse labs into sections using regex. Further string manipulation will determine how many optional and required points there are per section based on the rubric tags. The function will return a table of totals so that students can plan how many optionals they wish to complete.

- **Function 3**: `check_mechanics()` conduct and print a series of mechanics checks to screen. For example, the function will
  - Check that you have included a Github repo link;
  - Check that you have pushed the latest version; and
  - Check that you have at least three commits.

The package data will include a directory of public and/or dummy lab files (.ipynb and .Rmd). Private or unpublished lab files will not be committed to the repository.

To the authors' knowledge, no package yet exists in the R ecosystem that serves this specific purpose. However, several existing packages will be used to power the functionality of `labzenr`, including `gert` and `usethis`.Some other python repos and past assignments may be used as inspiration, such as the parsing work done in the _throughput database_ in DSCI 513.

This is a port of python [labzen package](https://github.com/UBC-MDS/labzen) written by the same authors.

## Installation

You can install the released version of labzenr from [Github](https://github.com/UBC-MDS/labzenr) with:

``` r
install.packages("devtools")
devtools::install_github("UBC-MDS/labzenr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(labzenr)
## To be written
```
## Code of Conduct

Please note that the labzenr project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
