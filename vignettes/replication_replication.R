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

## ------------------------------------------------------------------------
its <- cbn_get_items("WEFAT", 2)
its_vecs <- cbn_item_vectors[its$Word, ]
res <- wefat_boot(its, its_vecs, x_name = "AndrogynousNames",
                  a_name = "MaleAttributes", b_name = "FemaleAttributes",
                  se.calc = "quantile")

## ------------------------------------------------------------------------
data(cbn_gender_name_stats)
head(cbn_gender_name_stats)

res <- merge(res, cbn_gender_name_stats, 
             by.x = "AndrogynousNames", by.y = "name")

## ---- eval = FALSE-------------------------------------------------------
#  library(gender)
#  
#  names <- c("Hugh", "Pugh", "Barney")
#  gender_name_stats <- gender(names)

## ------------------------------------------------------------------------
ggplot(res, aes(x = proportion_male, y = diff)) +
  geom_hline(yintercept = 0, alpha = 0.5, color = "grey") + 
  geom_point() +
  geom_text(aes(label = AndrogynousNames), hjust = "left", nudge_x = 0.005) +
  xlim(0, 1) +
  xlab("Population proportion male") +
  ylab("Cosine difference (male - female)")

## ------------------------------------------------------------------------
cor.test(res$diff,res$proportion_male)

