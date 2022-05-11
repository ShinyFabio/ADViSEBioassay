
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ADViSEBioassay

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of ADViSEBioassay is to …

## Installation

ADViSEBioassay is a stand-alone application implemented using the R
language (R \> 4.0) and the Shiny libraries. It can be installed as any
other R package on several operating systems (Windows, macOS and Linux).
Before installing the package you have to perform few supplementary
steps based on your operating systems:

-   **Windows (tested on Windows 10 64bit)** 

Before installing the package you need also to install Rtools from the
following link:

<https://cran.r-project.org/bin/windows/Rtools>.

-   **MacOS**  
    If you are on MacOs run the following codes in the console:

``` r
brew install imagemagick@6
brew install cairo
```

-   **Ubuntu (tested on 18.04).**  

If you are on Ubuntu run the following codes in the console:

    sudo apt install build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev
    sudo apt-get install libcairo2-dev
    sudo apt-get install libxt-dev
    sudo apt install libmagick++-dev
    sudo apt-get install libc6
    sudo apt-get install libnlopt-dev

After that, open RStudio, check if you have already installed the
`{devtools}` package and run the following code:

``` r
devtools::install_github("ShinyFabio/ADViSEBioassay")
```

Be careful that if you need to install many packages and you decide to
use compilation, the process could take a lot depending on your hardware
and operating system.

## Usage

Once the installation is completed, run:

``` r
library("ADViSEBioassay")
run_app()
```

## Funding

…ADViSE
