# Pairwise phylogenetic/morphological distance test
# Muschick et al 2012, Current Biology
# Currently only implements Euclidean distance as in the paper
# but could incorporate other measures of disparity

# -----------------------------------------------------------------------------
# Functions to get phylogenetic distances

# Create a dataframe from a phylogenetic distance matrix so that it can be
# easily added to other data for plots and analysis
# Output columns are the species pairs and the phenetic distance
# Input is a phylogenetic distance matrix from `ape::cophenetic.phylo`
get_phylo_dist_data <- function(phylo_matrix) {
  data.frame(expand.grid(speciesA = dimnames(phylo_matrix)[[1]],
	                       speciesB = dimnames(phylo_matrix)[[2]]),
             phylodist = c(phylo_matrix))

}

# -----------------------------------------------------------------------------
# Functions to get morphological distances

# Create morphological distance matrix, including only columns with numbers
# Uses full triangle of comparisons to ensure it matches to phylogeny
get_morpho_matrix <- function(data, variable_list) {
  dist(data[variable_list], upper = TRUE, diag = TRUE)
}

# Flatten morphological distance matrix so it can be added to a dataframe
flatten_morpho_matrix <- function(morpho_matrix) {
  melt(as.matrix(morpho_matrix))
}

# Create and flatten morphological distance matrix
get_flat_morpho_matrix <- function(data, variable_list) {
  flatten_morpho_matrix(get_morpho_matrix(data, variable_list))
}

# Create a dataframe from a flat morphological distance matrix
# Output columns are the species pairs and the morphological distance
# Input is a melted morphological distance matrix from `flatten_morpho_matrix`
# Plus the original morphology dataframe to extract species names
get_morpho_dist_data <- function(data, species_col, flat_morpho_matrix) {
  data.frame(expand.grid(speciesA = data[, species_col],
	                       speciesB = data[, species_col]),
             morphodist = flat_morpho_matrix$value)
}

# -----------------------------------------------------------------------------
# Function to tidy the output

# Merge the phylogenetic and morphological distance matrices
merge_data <- function(phylo_dist_data, morpho_dist_data) {
  merge(phylo_dist_data, morpho_dist_data, by = c("speciesA", "speciesB"))
}

# Remove comparisons of species to same species - not relevant for analyses
remove_intraspecific <- function(merged_data) {
  merged_data[which(merged_data$phylodist != 0 & merged_data$morphodist != 0), ]
}

# Remove duplicates - avoids pseudoreplication
remove_duplicates <- function(merged_data) {
  merged_data[!duplicated(cbind(merged_data$phylodist, merged_data$morphodist)), ]
}

# Tidy phylogenetic and morphological distance matrices
tidy_data <- function(phylo_dist_data, morpho_dist_data) {
  tidy <- merge_data(phylo_dist_data, morpho_dist_data)
  tidy <- remove_intraspecific(tidy)
  tidy <- remove_duplicates(tidy)
}

# -----------------------------------------------------------------------------
# Main function
# Describe in more detail

pairwise_converge <- function(phy, data, variables, species, 
                              warn_dropped = FALSE) {

# Check inputs are correct
if (!is.data.frame(data))
    stop("'data' must be an object of class 'data.frame'")

if (!inherits(phy, "phylo"))
    stop("'phy' must be an object of class 'phylo'")

# Extract list of variables for morphological distances
# and species names
variable_list <- id_variables(data, variables)
species_col <- id_variables(data, species)

# Tidy up data and tree
# Remove incomplete data, missing species and reorder to match tree
data <- remove_incomplete_data(data, variable_list)
data <- remove_missing_species_data(phy, data, species_col)
phy <- remove_missing_species_tree(phy, data, species_col)

  if (warn_dropped) {
    tree_not_data <- id_missing_tree(phy, data, species_col)
    data_not_tree <- id_missing_data(phy, data, species_col)
  }

data <- sort_species_data(phy, data, species_col)

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

# -----------------------------------------------------------------------------

# Need to run Brownian simulations for comparison
# Create p value/bootstrapping procedure
# simulate shape coordinates? geomorph
# Plotting - with and without binning?
# Testing needed use testthat
