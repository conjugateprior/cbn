% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weats.R
\name{cbn_get_items}
\alias{cbn_get_items}
\title{Get the Items in a Study}
\usage{
cbn_get_items(type = c("all", "WEAT", "WEFAT"), number = 1)
}
\arguments{
\item{type}{"all" (the default), "WEAT", or "WEFAT"}

\item{number}{study number (default: 1) Ignored if \code{type} = "all"}
}
\value{
a data frame of items in columns or a vector of all items
}
\description{
Returns a data frame containing the items from one of the studies
(WEAT1 through WEAT10 or WEFAT1 or WEFAT2) or a vector containing
all items from all studies if \code{type} == "all".
}
