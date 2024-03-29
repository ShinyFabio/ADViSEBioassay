% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_TREM2.R
\name{read_TREM2}
\alias{read_TREM2}
\title{read_TREM2}
\usage{
read_TREM2(file_explist, file_target, filter.na = "Product")
}
\arguments{
\item{file_explist}{The row of the experiment list of that sample (i.e. the experiment list filtered by EXPID)}

\item{file_target}{The row of the target file of that sample (i.e. the target file filtered by EXPID)}

\item{filter.na}{A column where the NA filtering is performed. By default is Product.}
}
\description{
\code{read_TREM2} Read D1 data
}
