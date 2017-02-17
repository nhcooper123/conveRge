# Required packages
library(ape)
library(reshape2)

# Extract column numbers for a vector of variable names
id_variables <- function(data, variables) {
  sapply(variables, function(x)
         which(names(data) == x))
}

# Create a dataframe from a phylogenetic distance matrix
# Output columns are the species pairs and the phenetic distance
# Input is a phylogenetic distance matrix from `cophenetic.phylo`
get_phylo_dist_data <- function(phylo_matrix) {
  data.frame(expand.grid(speciesA = dimnames(phylo_matrix)[[1]],
	                       speciesB = dimnames(phylo_matrix)[[2]]),
             phylodist = c(phylo_matrix))

}

# Create morphological distance matrix, including only columns containing numbers
get_morpho_matrix <- function(data, variable_list) {
  dist(data[variable_list], upper = TRUE, diag = TRUE)
}

# Flatten morphological distance matrix to add to dataframe
flatten_morpho_matrix <- function(morpho_matrix) {
  melt(as.matrix(morpho_matrix))
}

# Create and flatten morphological distance matrix
get_flat_morpho_matrix <- function(data, variable_list) {
  flatten_morpho_matrix(get_morpho_matrix(data, variable_list))
}

# Create a dataframe from a flat morphological distance matrix
# Output columns are the species pairs and the morphological distance
# Input is a melted morphological distance matrix from `flatten.morpho.matrix`
# Plus the original morphology dataframe to extract species
get_morpho_dist_data <- function(data, species_col, flat_morpho_matrix) {
  data.frame(expand.grid(speciesA = data[, species_col],
	                       speciesB = data[, species_col]),
             morphodist = flat_morpho_matrix$value)
}

# Merge the phylogenetic and morphological distance matrices
merge_data <- function(phylo_dist_data, morpho_dist_data) {
  merge(phylo_dist_data, morpho_dist_data, by = c("speciesA", "speciesB"))
}

# Remove comparisons of species to same species
remove_intraspecific <- function(merged_data) {
  merged_data[which(merged_data$phylodist != 0 & merged_data$morphodist != 0), ]
}

# Remove duplicates
remove_duplicates <- function(merged_data) {
  merged_data[!duplicated(cbind(merged_data$phylodist, merged_data$morphodist)), ]
}

# Tidy phylogenetic and morphological distance matrices
tidy_data <- function(phylo_dist_data, morpho_dist_data) {
  tidy <- merge_data(phylo_dist_data, morpho_dist_data)
  tidy <- remove_intraspecific(tidy)
  tidy <- remove_duplicates(tidy)
}

# Main function
# Describe in more detail

pairwise <- function(phy, morpho_data, variables, species) {

# Check inputs are correct
if (!is.data.frame(morpho_data))
    stop("'data' must be an object of class 'data.frame'")

if (!inherits(phy, "phylo"))
    stop("'phy' must be an object of class 'phylo'")


# Check these are all in same order
# Probably need to order data so it matches the tree
# Use tree data perhaps?

# Extract list of variables for morphological distances
# and species names
variable_list <- id_variables(morpho_data, variables)
species_col <- id_variables(morpho_data, species)

# Create morphological distance matrix, including only columns with numbers
flat_morpho_matrix <- get_flat_morpho_matrix(morpho_data, variable_list)
morpho_dist_data <- get_morpho_dist_data(morpho_data, species_col,
                                         flat_morpho_matrix)

# Extract all phylogenetic distances from phylogeny
phylo_dist_data <- get_phylo_dist_data(cophenetic.phylo(phy))

# Combine the two matrices and remove duplicates/intraspecific comparisons
distances <- tidy_data(phylo_dist_data, morpho_dist_data)

return(distances)

}

# Need to run Brownian simulations for comparison
# Create p value/bootstrapping procedure
# simulate shape coordinates? geomorph
# Plotting - with and without binning?
# Testing needed use testthat
