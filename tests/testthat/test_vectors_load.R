context("Vectors")
library(cbn)

oldvecfile <- cbn_get_vectorfile_location()
vfile <- tempfile()
writeLines("sausage 0.1 0.2 0.3 1.4\nroll 12.0 3 3 0.00001\n", con = vfile)

test_that("set and reset the environment variable", {
  fullvfile <- normalizePath(vfile)
  cbn_set_vectorfile_location(vfile)
  expect_true(Sys.getenv("CBN_VECTORS_LOCATION") == fullvfile)
  cbn_set_vectorfile_location(oldvecfile)
  expect_true(Sys.getenv("CBN_VECTORS_LOCATION") == oldvecfile)
  Sys.setenv(CBN_VECTORS_LOCATION=oldvecfile) #manually in case that failed
})

test_that("vectors read in properly", {
  cbn_set_vectorfile_location(vfile)
  vecs <- cbn_extract_word_vectors(c("roll", "sausage"))
  expect_equal(rownames(vecs), c("roll", "sausage"))
  expect_equal(dim(vecs), c(2, 4))
  expect_equal(vecs[2,1], c(sausage = 0.1))
  cbn_set_vectorfile_location(oldvecfile)
})
