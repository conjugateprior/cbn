


#' WEFAT via simple item bootstrap
#'
#' A simple bootstrap for the WEFAT calculations.  The statistic
#' of interest is the difference between the cosine of each word in condition
#' \code{x_name} e.g. "Careers", to the mean vector of condition \code{a_name},
#' e.g. "MaleAttributes" and the mean vector from condition \code{b_name},
#' e.g. "FemaleAttributes".
#'
#' Uncertainty is quantified by bootstrapping each set of
#' item vectors. That is, in each of the \code{b} bootstrap samples,
#' vectors in the \code{a_name} condition and
#' vectors in the \code{b_name} condition are
#' resampled (independently) with replacement, and the difference between
#' the cosine of a target word and the mean of the \code{a_name}
#' vectors and cosine of a target word and the mean of the \code{b_name}
#' is recorded.  The bootstrap sampling distribution of this difference of
#' cosines statistic is summarized in the outpu by an approximate
#' 95% confidence interval defined as either twice the standard deviation of the
#' statistic across bootstrap samples if \code{se.calc} is "sd", or as the
#' 0.025 and 0.975 quantiles of the bootstrap sampling distribution
#' if \code{se.calc} is "quantile".
#'
#' If \code{se.calc} is "quantile" the data frame returned has an extra column
#' containing the median of the statistic in the bootstrap samples. This should not
#' be too far from the original statistic.
#'
#' The output of this function is sorted by the value of the difference of
#' cosines statistic. This direction is arbitrary, but if you wish to reverse
#' the ordering just swap the values of \code{a_name} for \code{b_name} when
#' calling it.
#'
#' Note that this is not the statistic reported in the original paper.
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
#' @importFrom stats median quantile sd
#'
#' @examples
#' its <- cbn_get_items("WEFAT", 1)
#' its_vecs <- cbn_get_item_vectors("WEFAT", 1)
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

#' WEAT via simple item bootstrap
#'
#' A simple bootstrap for the WEAT calculations.  The statistic
#' of interest is an average difference of average differences.
#'
#' Schematically, the statistic is the average value of
#'
#' (cosine(x names, a words) - cosine(x names, b words)) -
#' (cosine(y names, a words) - cosine(y names, b words))
#'
#' If a denotes a set of 'Pleasant' and b denotes a set of 'Unpleasant' words,
#' x are names of 'Insects', and y are names of 'Flowers' (as in WEAT 1)
#' then the statistic will take positive values when flowers are more pleasant
#' than insects. That is, when the degree to which flower names are more similar
#' to pleasant versus unpleasant words is stronger than the degree to which
#' insect names are more similar to pleasant versus unpleasant words.
#'
#' Uncertainty is quantified by bootstrapping each set of
#' item vectors. That is, in each of the \code{b} bootstrap samples,
#' vectors in each condition (\code{a_name}, \code{b_name},
#' \code{x_name} and \code{y_name}) are
#' separately resampled with replacement, and the statistic is
#' computed.  The bootstrap sampling distribution of this statistic
#' is summarized in the outpu by an approximate
#' 95% confidence interval defined as either twice the standard deviation of the
#' statistic across bootstrap samples if \code{se.calc} is "sd", or as the
#' 0.025 and 0.975 quantiles of the bootstrap sampling distribution
#' if \code{se.calc} is "quantile".
#'
#' If \code{se.calc} is "quantile" the data frame returned has an extra column
#' containing the median of the statistic in the bootstrap samples. This should not
#' be too far from the original statistic.
#'
#' The sign of the statistic is arbitrary. If you wish to reverse
#' the ordering just swap the values of \code{a_name} for \code{b_name}
#' or \code{x_name} and \code{y_name} when calling it.
#'
#' Note that this is not the statistic reported in the original paper.  This
#' bootstraps within each target categories (x and y) and within each attribute
#' category (a and b).
#'
#' @param items information about the items, typically from
#'              \code{\link{cbn_get_items}}
#' @param vectors a matrix of word vectors for all the study items
#' @param x_name the \emph{name} of the target item condition, e.g. "Flowers"
#'               in WEAT 1
#' @param y_name the \emph{name} of the target item condition, e.g. "Insects"
#'               in WEAT 1
#' @param a_name the name of the first condition, e.g. "Pleasant" in
#'               WEAT 1
#' @param b_name the name of the second condition, e.g. "Unpleasant" in
#'               WEAT 1
#' @param b number of bootstrap samples. Defaults to 300.
#' @param se.calc how to compute lower and upper bounds on an approximate 95%
#'                interval for the difference of differences of cosines statistic.
#'                "se" (default) or "quantile".
#'
#' @return a data frame with first column the
#'         difference of differences of cosines statistic, the second and third
#'         columns the lower and upper bounds of an approximate 95% confidence
#'         interval from the bootstrapped statistic.  If \code{se.calc} is "quantile",
#'         the fourth column is the median value of the statistic across
#'         bootstrap samples.
#' @export
#' @importFrom stats median quantile sd
#'
#' @examples
#' its <- cbn_get_items("WEAT", 1)
#' its_vecs <- cbn_get_item_vectors("WEAT", 1)
#' res <- weat_boot(its, its_vecs,
#'                  x_name = "Flowers", y_name = "Insects",
#'                  a_name = "Pleasant", b_name= "Unpleasant",
#'                  se.calc = "quantile")
#' res
weat_boot <- function(items, vectors, x_name, y_name, a_name, b_name,
                      b = 300, se.calc = c("sd", "quantile")){
  se.calc <- match.arg(se.calc)
  x_words <- items$Word[items$Condition == x_name]
  x_vecs <- vectors[x_words, ]
  y_words <- items$Word[items$Condition == y_name]
  y_vecs <- vectors[y_words, ]
  a_v <- vectors[items$Condition == a_name, ]
  b_v <- vectors[items$Condition == b_name, ]

  # point estimate
  orig_ax <- mean(cbn_cosine(colMeans(a_v), x_vecs))
  orig_bx <- mean(cbn_cosine(colMeans(b_v), x_vecs))
  orig_ay <- mean(cbn_cosine(colMeans(a_v), y_vecs))
  orig_by <- mean(cbn_cosine(colMeans(b_v), y_vecs))
  orig <- (orig_ax - orig_bx) - (orig_ay - orig_by)

  reps <- rep(NA, b)
  for (i in 1:b) {
    # resample everything independently
    a_repl <- a_v[sample(1:nrow(a_v), replace = TRUE), ]
    b_repl <- b_v[sample(1:nrow(b_v), replace = TRUE), ]
    x_repl <- x_vecs[sample(1:nrow(x_vecs), replace = TRUE), ]
    y_repl <- y_vecs[sample(1:nrow(y_vecs), replace = TRUE), ]

    repl_ax <- mean(cbn_cosine(colMeans(a_repl), x_repl))
    repl_bx <- mean(cbn_cosine(colMeans(b_repl), x_repl))
    repl_ay <- mean(cbn_cosine(colMeans(a_repl), y_repl))
    repl_by <- mean(cbn_cosine(colMeans(b_repl), y_repl))
    reps[i] <- (repl_ax - repl_bx) - (repl_ay - repl_by)
  }

  if (se.calc == "sd") {
    se <- sd(reps) # just one row
    df <- data.frame(diff = orig,
                     lwr = orig - 2 * se,
                     upr = orig + 2 * se)
  } else {
    qs <- quantile(reps, probs = c(0.025, 0.975))
    df <- data.frame(diff = orig,
                     lwr = qs[1],
                     upr = qs[2],
                     median = median(reps))
  }
  rownames(df) <- NULL
  df
}

#' WEAT Permutation Test
#'
#' The statistic computed by this function is the mean cosine
#' similarity of each x item to the a attributes minus the mean cosine to the
#' b attributes, summed over x items subtracted for the same quantity
#' computed for the y items.  See the paper for details of the statistic,
#' and the effect size.
#'
#' The p value is constructed by permuting the assignment of words to the x and
#' y conditions. (The a and b attribute items are fixed.)  The p value is the
#' proportion of times the statistic computed on the permuted labels is greater
#' than the value of the statistic that is observed.
#'
#' @param items information about the items, typically from
#'              \code{\link{cbn_get_items}}
#' @param vectors a matrix of word vectors for all the study items, typically
#'                from \code{\link{cbn_get_item_vectors}}
#' @param x_name the \emph{name} of the target item condition, e.g. "Flowers"
#'               in WEAT 1
#' @param y_name the \emph{name} of the target item condition, e.g. "Insects"
#'               in WEAT 1
#' @param a_name the name of the first condition, e.g. "Pleasant" in
#'               WEAT 1
#' @param b_name the name of the second condition, e.g. "Unpleasant" in
#'               WEAT 1
#' @param b number of bootstrap samples. Defaults to 1000.
#'
#' @return a data frame with first column the
#'         statistic, the second column the effect size, and the third column
#'         permutation test p value.
#' @export
#' @importFrom stats sd
#'
#' @examples
#' its <- cbn_get_items("WEAT", 1)
#' its_vecs <- cbn_get_item_vectors("WEAT", 1)
#' res <- weat_perm(its, its_vecs,
#'                  x_name = "Flowers", y_name = "Insects",
#'                  a_name = "Pleasant", b_name= "Unpleasant")
#' res
weat_perm <- function(items, vectors, x_name, y_name, a_name, b_name, b = 1000){

  x_words <- items$Word[items$Condition == x_name]
  y_words <- items$Word[items$Condition == y_name]
  a_words <- items$Word[items$Condition == a_name]
  b_words <- items$Word[items$Condition == b_name]

  pre_cos <- cbn_cosine(vectors) # Compute just once

  # point estimate straight from the paper
  S_xab <- apply(pre_cos[x_words, a_words], 1, mean) -
           apply(pre_cos[x_words, b_words], 1, mean)
  S_yab <- apply(pre_cos[y_words, a_words], 1, mean) -
           apply(pre_cos[y_words, b_words], 1, mean)
  S_xy_denom <- apply(pre_cos[c(x_words, y_words), a_words], 1, mean) -
                apply(pre_cos[c(x_words, y_words), b_words], 1, mean)
  S_xyab <- sum(S_xab) - sum(S_yab)
  effect <- (mean(S_xab) - mean(S_yab)) / sd(S_xy_denom)

  lx <- length(x_words)
  ly <- length(y_words)
  reps <- rep(NA, b)
  for (i in 1:b) {
    shuf <- sample(c(x_words, y_words))
    repl_x_words <- shuf[1:lx]
    repl_y_words <- shuf[(lx + 1):(lx + ly)]
    S_xab <- apply(pre_cos[repl_x_words, a_words], 1, mean) -
      apply(pre_cos[repl_x_words, b_words], 1, mean)
    S_yab <- apply(pre_cos[repl_y_words, a_words], 1, mean) -
      apply(pre_cos[repl_y_words, b_words], 1, mean)
    reps[i] <- sum(S_xab) - sum(S_yab)
  }
  p_val <- sum(reps > S_xyab) / length(reps)

  df <- data.frame(S_xyab = S_xyab,
                   d = effect,
                   p_value = p_val)
  rownames(df) <- NULL
  df
}

