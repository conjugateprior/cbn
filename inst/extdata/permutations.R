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
