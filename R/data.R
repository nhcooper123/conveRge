#' Morphological data for 23 odontocete (toothed whale) species
#'
#' A dataset containing the first five principal components of
#' a principal components analysis on Procrustes aligned coordinates taken from 
#' photographs of odontocete mandibles. These are species averages taken from 
#' specimens for 23 odontocete species. The data were collected to test for 
#' convergence in "river dolphins" versus other species. 
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{species}{species names}
#'   \item{river}{whether species is a "river dolphin" or not}
#'   \item{pc1}{First principal component}
#'   \item{pc2}{Second principal component}
#'   \item{pc3}{Third principal component}
#'   \item{pc4}{Fourth principal component}
#'   \item{pc5}{Fifth principal component}
#'   ...
#' }
#' @source Collected by Charlotte Page, MSci student (2016-2017)
"odontocete_data"

#' Phylogeny of odontocetes (toothed whales) 
#'
#' A rooted phylogeny with branch lengths for 73 odontocete species.
#'
#' @format A phylo object with 73 tips and 72 internal nodes.
#'   ...
#' }
#' @source Steeman, M.E., M.B. Hebsgaard, R.E. Fordyce, S.W.Y. Ho, 
#'   D.L. Rabosky, R. Nielsen, C. Rahbek, H. Glenner, M.V. Sorensen, 
#'   E. Willerslev. 2009. Systematic Biology. 58: 573-585. 
#'   DOI: 10.1093/sysbio/syp060
"odontocete_tree"