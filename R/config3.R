##############################################################################
#                                                                            #
#                         LATTICE CONFIGURATION                              #
#                                                                            #
##############################################################################

#' @title
#' Configure the initial state of the square lattice
#'
#' @description
#' Initializes the lattice, setting all its elements to be random integers from 0 to 4
#'
#' @param lattice The lattice that has been initially built
#'
#' @usage
#' config3(lattice)
#'
#' @examples
#' init(50)
#' lattice <- config3(lattice)
#' lattice
#'
#' @export
#'

config3 <- function(Lattice){
  l <- NROW(Lattice)
  for(i in 1:l){
    for(j in 1:l) {
      Lattice[i, j] <- sample(0:4, 1)
    }
  }
  return(Lattice)
}
