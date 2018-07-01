we1 <- cbn_get_items("WEAT")
gg <- data.frame(we1, cbn_item_vectors[we1$Word,])

# more descriptively
plot(hclust(dist(-cbn_item_cosines[we1$Word, we1$Word] + 1)))

# cosine of vector mean
by_cond <- split(gg, gg$Condition)
avs <- lapply(by_cond, function(x){ colMeans(x[, 4:ncol(x)]) })
av_mat <- do.call(rbind, avs)
cbn_cosine(av_mat)

# complete cosine set to do permutations on
#
