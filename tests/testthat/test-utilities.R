context("utilities.R")

# Read in test data 
data("odontocete_tree")
data("odontocete_data")

test_that("id_variables selects the correct number of variables", {
  expect_equal(length(id_variables(odontocete_data, c("pc1"))), 1)
  expect_equal(length(id_variables(odontocete_data, c("pc1", "pc5"))), 2)
})

test_that("id_variables stops if variable names do not match", {
  expect_error(id_variables(odontocete_data, c("PC1")))
})


