


#' WEFAT via simple item bootstrap
#'
#' A simple bootstrap for the WEFAT calculations.  The statistic
#' of interest is the difference between the cosine of each word in condition
#' \code{x_name} e.g. "Careers", to the mean vector of condition \code{a_name},
#' e.g. "MaleAttributes" and the mean vector from condition \code{b_name},
#' e.g. "FemaleAttributes".
#'
#' Uncertainty is quantified by bootstrapping each set
#' item vectors. That is, for each of the \code{b} bootstrap samples,
#' vectors in the \code{a_name} condition and
#' vectors in the \code{b_name} condition are
#' resampled (independently) with replacement, and the difference of cosines
#' statstic is recomputed.  The bootstrap sampling distribution of the
#' statistic is summarized by an approximate
#' 95% confidence interval defined as twice the standard deviation of the
#' statistic across bootstrap samples if \code{se.calc} is "sd", or as the
#' 0.025 and 0.975 quantiles of the statistic if \code{se.calc} is "quantile".
#'
#' If \code{se.calc} is "quantile" the data frame returned has an extra column
#' with the median of the statistic in the bootstrap samples. This should not
#' be too far from the original statistic.
#'
#' The output of this function is sorted by the value of the difference of
#' cosines statistic. This direction is arbitrary, but if you wish to reverse
#' the ordering just swap the values of \code{a_name} for \code{b_name} when
#' calling it.
#'
#' @param items information about the items, typically from
#'              \code{\link{cbn_get_items}}
#' @param vectors a matrix of word vectors for the study
#' @param x_name the \emph{name} of the target item condition, e.g. "Careers"
#'               in WEFAT 1
#' @param a_name the name of the first condition, e.g. "MaleAttributes" in
#'               WEFAT 1 and 2
#' @param b_name the name of the second condition, e.g. "FemaleAttributes" in
#'               WEFAT 1 and 2
#' @param b number of bootstrap samples. Defaults to 300.
#' @param se.calc how to compute lower and upper bounds on an approximate 95%
#'                interval for the difference of cosines statistic. "se"
#'                (default) or "quantile".
#'
#' @return a data frame with first column \code{x_name}, second column the
#'         difference of cosines statistic, third and fourth columns the
#'         lower and upper bounds of an approximate 95% confidence interval
#'         from the bootstrapped statistic. If \code{se.calc} is "quantile",
#'         the fifth column is the median value of the statistic across
#'         bootstrap samples. The data frame is sorted by the second column.
#' @export
#'
#' @examples
#' its <- cbn_get_items("WEFAT", 1)
#' its_vecs <- cbn_item_vectors[its$Word, ]
#' res <- wefat_boot(its, its_vecs, x_name = "Careers",
#'                   a_name = "MaleAttributes", b_name = "FemaleAttributes",
#'                   se.calc = "quantile")
wefat_boot <- function(items, vectors, x_name, a_name, b_name,
                       b = 300, se.calc = c("sd", "quantile")){
  se.calc <- match.arg(se.calc)
  x_words <- items$Word[items$Condition == x_name]
  x_vecs <- vectors[x_words, ]
  repl <- matrix(NA, nrow = length(x_words), ncol = b)
  # point estimate
  a_v <- vectors[items$Condition == a_name,]
  b_v <- vectors[items$Condition == b_name,]
  orig_a <- cbn_cosine(colMeans(a_v), x_vecs)
  orig_b <- cbn_cosine(colMeans(b_v), x_vecs)
  orig <- orig_a - orig_b

  for (i in 1:b) {
    a_repl <- a_v[sample(1:nrow(a_v), replace = TRUE), ]
    b_repl <- b_v[sample(1:nrow(b_v), replace = TRUE), ]
    sims_a <- cbn_cosine(colMeans(a_repl), x_vecs)
    sims_b <- cbn_cosine(colMeans(b_repl), x_vecs)
    # statistic: difference of cosines
    repl[, i] <- sims_a - sims_b
  }
  if (se.calc == "sd") {
    se <- apply(repl, 1, sd)
    tmp <- matrix(c(orig - 2 * se, orig + 2 * se), ncol = 2)
  } else {
    tmp <- t(apply(repl, 1, quantile, probs = c(0.025, 0.975)))
  }
  names(tmp) <- c("lwr", "upr")

  df <- data.frame(x = x_words, diff = orig, tmp)
  if (se.calc == "quantile")
    df$median <- apply(repl, 1, median)

  colnames(df)[1:4] <- c(x_name, "diff", "lwr", "upr")
  df <- df[order(df$diff), ]
  rownames(df) <- NULL
  df
}
