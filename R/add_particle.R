##############################################################################
#                                                                            #
#                         PARTICLE ADDITION AT A SITE                        #
#                                                                            #
##############################################################################

#' @title
#' Addition of a single sand particle at a random site of the square lattice
#'
#' @description
#' This function randomly selects a site from the square lattice and adds a sand particle to that particular site.
#'
#' @param Lattice The configured lattice
#'
#' @usage
#' add_particle(Lattice)
#'
#' @examples
#' init(5)
#' lattice <- config3(lattice)
#' lattice
#' lattice <- add_particle(lattice)
#' lattice
#'
#' @export
#'

add_particle <- function(Lattice){
  L <- NROW(Lattice)
  i <- sample(1:L, 1)
  j <- sample(1:L, 1)
  Lattice[i,j] <- Lattice[i, j]+1
  return (Lattice)
}
