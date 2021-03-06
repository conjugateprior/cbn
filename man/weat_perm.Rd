% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statistics.R
\name{weat_perm}
\alias{weat_perm}
\title{WEAT Permutation Test}
\usage{
weat_perm(items, vectors, x_name, y_name, a_name, b_name, b = 1000)
}
\arguments{
\item{items}{information about the items, typically from
\code{\link{cbn_get_items}}}

\item{vectors}{a matrix of word vectors for all the study items, typically
from \code{\link{cbn_get_item_vectors}}}

\item{x_name}{the \emph{name} of the target item condition, e.g. "Flowers"
in WEAT 1}

\item{y_name}{the \emph{name} of the target item condition, e.g. "Insects"
in WEAT 1}

\item{a_name}{the name of the first condition, e.g. "Pleasant" in
WEAT 1}

\item{b_name}{the name of the second condition, e.g. "Unpleasant" in
WEAT 1}

\item{b}{number of bootstrap samples. Defaults to 1000.}
}
\value{
a data frame with first column the
        statistic, the second column the effect size, and the third column
        permutation test p value.
}
\description{
The statistic computed by this function is the mean cosine
similarity of each x item to the a attributes minus the mean cosine to the
b attributes, summed over x items subtracted for the same quantity
computed for the y items.  See the paper for details of the statistic,
and the effect size.
}
\details{
The p value is constructed by permuting the assignment of words to the x and
y conditions. (The a and b attribute items are fixed.)  The p value is the
proportion of times the statistic computed on the permuted labels is greater
than the value of the statistic that is observed.
}
\examples{
its <- cbn_get_items("WEAT", 1)
its_vecs <- cbn_get_item_vectors("WEAT", 1)
res <- weat_perm(its, its_vecs,
                 x_name = "Flowers", y_name = "Insects",
                 a_name = "Pleasant", b_name= "Unpleasant")
res
}
