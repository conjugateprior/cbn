## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(cbn)

library(ggplot2)
theme_set(theme_minimal())

## ------------------------------------------------------------------------
its <- cbn_get_items("WEFAT", 1)
its
its_vecs <- cbn_item_vectors[its$Word, ]
dim(its_vecs)

## ------------------------------------------------------------------------
res <- wefat_boot(its, its_vecs, x_name = "Careers",
                  a_name = "MaleAttributes", b_name = "FemaleAttributes",
                  se.calc = "quantile")

head(res)

## ------------------------------------------------------------------------
ggplot(res, aes(x = median, y = 1:nrow(res))) +
  geom_point(col = "grey") +
  geom_point(aes(x = diff)) +
  geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0) +
  geom_text(aes(x = upr, label = Careers), hjust = "left", nudge_x = 0.005) +
  xlim(-0.25, 0.25) +
  ylab("Careers") +
  xlab("Cosine difference (male - female)")

