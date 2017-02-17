context("utilities.R")

# Load ape library
library(ape)

# Read in test data 
data("odontocete_tree")
data("odontocete_data")

# Create test dataset/tree with missing data
odontocete_data2 <- odontocete_data
odontocete_data2[5:7, 3] <- NA
odontocete_data2[8, 2] <- NA
odontocete_tree2 <- drop.tip(odontocete_tree, "Delphinus_delphis")

# Create sorted cleaned data and tree
tree <- remove_missing_species_tree(odontocete_tree, odontocete_data, 1)
data <- remove_missing_species_data(tree, odontocete_data, 1)
data_sort <- sort_species_data(tree, data, 1)

# -----------------------------------------------------------------------------
# Tests

# id_variables
test_that("id_variables selects the correct number of variables", {
  expect_equal(length(id_variables(odontocete_data, c("pc1"))), 1)
  expect_equal(length(id_variables(odontocete_data, c("pc1", "pc5"))), 2)
})

test_that("id_variables stops if variable names do not match", {
  expect_error(id_variables(odontocete_data, c("PC1")))
  expect_error(id_variables(odontocete_data, c("PC1", "pc1")))
})

# remove_incomplete_data
test_that("remove_incomplete_data removes data", {
  expect_equal(dim(remove_incomplete_data(odontocete_data2, c(1, 3)))[[1]], 20)
  expect_equal(dim(remove_incomplete_data(odontocete_data2, c(1, 2, 3)))[[1]], 19)
})

test_that("remove_incomplete_data leaves data when none is missing", {
expect_equal(dim(remove_incomplete_data(odontocete_data, c(1, 3)))[[1]], 23)
})

# id_missing_tree
test_that("id_missing_tree finds missing species", {
  expect_equal(length(id_missing_tree(odontocete_tree2, odontocete_data, 1)), 50)
})

# id_missing_data
test_that("id_missing_data finds missing species", {
  expect_equal(length(id_missing_data(odontocete_tree, odontocete_data, 1)), 0)
  expect_match(id_missing_data(odontocete_tree2, odontocete_data, 1), 
  	           "Delphinus_delphis")
})

# remove_missing_species_tree
test_that("remove_missing_species_tree removes the correct species", {
  expect_equal(length(remove_missing_species_tree(odontocete_tree, 
  	                                              odontocete_data, 1)$tip.label), 23)
  expect_equal(length(remove_missing_species_tree(odontocete_tree2, 
  	                                              odontocete_data, 1)$tip.label), 22)
  expect_equal(length(which(remove_missing_species_tree(odontocete_tree2, 
  	                                                    odontocete_data, 1)$tip.label 
                                                        == "Delphinus_delphis")), 0)
  expect_equal(length(which(remove_missing_species_tree(odontocete_tree2, 
  	                                                    odontocete_data, 1)$tip.label 
                                                        == "Sousa_plumbea")), 1)
})

# remove_missing_species_data
test_that("remove_missing_species_data removes the correct species", {
  expect_equal(length(remove_missing_species_data(odontocete_tree, 
  	                                              odontocete_data, 1)$species), 23)
  expect_equal(length(remove_missing_species_data(odontocete_tree2, 
  	                                              odontocete_data, 1)$species), 22)
  expect_equal(length(which(remove_missing_species_data(odontocete_tree2, 
  	                                                    odontocete_data, 1)$species 
                                                        == "Delphinus_delphis")), 0)
  expect_equal(length(which(remove_missing_species_data(odontocete_tree2, 
  	                                                    odontocete_data, 1)$species 
                                                        == "Sousa_plumbea")), 1)
})

# sort_species_data
test_that("sort_species_data sorts species correctly", {
  expect_match(as.character(data_sort$species[1]), tree$tip.label[1])
  expect_match(as.character(data_sort$species[14]), tree$tip.label[14])
  expect_match(as.character(data_sort$species[22]), tree$tip.label[22])
})
