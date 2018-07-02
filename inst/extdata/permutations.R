we1 <- cbn_get_items("WEFAT")
gg <- data.frame(we1, cbn_item_vectors[we1$Word,])

# more descriptively
# plot(hclust(dist(-cbn_item_cosines[we1$Word, we1$Word] + 1)))

# WEFAT cosine of vector mean
by_cond <- split(gg, gg$Condition)
avs <- lapply(by_cond[2:3], function(x){ colMeans(x[, 4:ncol(x)]) })
av_mat <- do.call(rbind, avs)
cbn_cosine(av_mat)

# complete cosine set to do permutations on
#
tech <- by_cond[[1]][,4:303]
rownames(tech) <- by_cond[[1]][,3]

ff <- apply(cbn_cosine(as.matrix(rbind(tech, av_mat)))[,51:52], 1,
            function(x) x[2] - x[1] )
ffs <- sort(ff)
plot(ffs, 1:50)
text(ffs, 1:50, names(ffs), pos = 4)



# WEFAT with no uncertainty
# cosine of x to a - cosine of x to b
wefat0 <- function(items, vectors, x_name, a_name, b_name){
  x_names <- items$Word[items$Condition == x_name]
  x_vecs <- vectors[x_names, ]
  a_vec <- colMeans(vectors[items$Condition == a_name,])
  b_vec <- colMeans(vectors[items$Condition == b_name,])
  mat <- as.matrix(rbind(a_vec, b_vec, x_vecs))
  sims <- cbn_cosine(mat)[3:nrow(mat), 1:2]
  dif <- apply(sims, 1, function(x) x[1] - x[2])
  df <- data.frame(x = names(dif), diff = dif)
  colnames(df)[1] <- x_name
  rownames(df) <- NULL
  df[order(df$diff),]
}

ff <- wefat0(cbn_get_items("WEFAT", 1),
             cbn_item_vectors[we1$Word,],
             "Careers", "MaleAttributes", "FemaleAttributes")

library(ggplot2)

theme_set(theme_minimal())
ggplot(ff, aes(x = diff, y = 1:nrow(ff), label = Careers)) +
  geom_point() +
  geom_text(hjust = "left", nudge_x = 0.005) +
  xlim(-0.2, 0.2) +
  ylab("Careers") +
  xlab("Cosine difference (male - female)")

# WEFAT more efficiently with bootstrapped items
# cosine of x to a - cosine of x to
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

its <- cbn_get_items("WEFAT", 1)
ff1 <- wefat1(its, cbn_item_vectors[its$Word, ],
              "Careers", "MaleAttributes", "FemaleAttributes",
              se.calc = "quantile")

# Plot
ggplot(ff1, aes(x = median, y = 1:nrow(ff1))) +
  geom_point(col = "grey") +
  geom_point(aes(x = diff)) +
  geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0) +
  geom_text(aes(x = upr, label = Careers), hjust = "left", nudge_x = 0.005) +
  xlim(-0.25, 0.25) +
  ylab("Careers") +
  xlab("Cosine difference (male - female)")


# AndrogynousNames

its <- cbn_get_items("WEFAT", 2)
ff1 <- wefat1(its, cbn_item_vectors[its$Word, ],
              "AndrogynousNames", "MaleAttributes", "FemaleAttributes",
              se.calc = "quantile")

# Plot
ggplot(ff1, aes(x = median, y = 1:nrow(ff1))) +
  geom_point(col = "grey") +
  geom_point(aes(x = diff)) +
  geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0) +
  geom_text(aes(x = upr, label = AndrogynousNames), hjust = "left", nudge_x = 0.005) +
  xlim(-0.25, 0.25) +
  ylab("Androgynous Names") +
  xlab("Cosine difference (male - female)")
