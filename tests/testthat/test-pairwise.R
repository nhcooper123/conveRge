context("pairwise.R")

test_that("input is a phylogeny")

expect_error(pairwise(), "'phy' must be an object of class 'phylo'")
