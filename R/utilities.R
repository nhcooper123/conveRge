# Utility functions for conveRge package

# -----------------------------------------------------------------------------
# Extract column numbers for a vector of variable names
id_variables <- function(data, variables) {
  check_variables(data, variables)
  sapply(variables, function(x)
         which(names(data) == x))
}

# Check variables given match those in the dataset
check_variables <- function(data, variables) {
  var_names <- length(variables)
  data_names <- length(which(!is.na(match(names(data), variables))))
  
if (var_names != data_names)
    stop("'variables' do not match the names in 'data'")
}

# -----------------------------------------------------------------------------
# Deal with missing data, and mismatches between data and tree species

# Remove rows where there are lines of missing data
remove_incomplete_data <- function(data, variables_col) {
  id <- complete.cases(data[, c(variables_col)])
  data <- data[id, ]
  return(data)
}

# ID species in tree that are not in the data
id_missing_tree <- function(phy, data, species_col) {
  setdiff(phy$tip.label, data[, species_col])
}

# ID species in data that are not in the tree
id_missing_data <- function(phy, data, species_col) {
  setdiff(data[, species_col], phy$tip.label)
}    

# Remove missing species from tree
remove_missing_species_tree <- function(phy, data, species_col) {
  tree_not_data <- id_missing_tree(phy, data, species_col)
  if (length(tree_not_data) > 0) {
    phy <- drop.tip(phy, tree_not_data)
  }
  return(phy)
}

# Remove missing species from data
remove_missing_species_data <- function(phy, data, species_col) {
  data_not_tree <- id_missing_data(phy, data, species_col)
  if (length(data_not_tree) > 0) {
    matches <- match(data[, species_col], data_not_tree, nomatch = 0)
    data <- subset(data, matches == 0)
  }
  return(data)
}

# Reorder data so species are in the same order as the phylogeny
sort_species_data <- function(phy, data, species_col) {
  data[match(phy$tip.label, data[, species_col]), ]
}