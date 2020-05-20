##############################################################################
#                                                                            #
#                         LATTICE CONFIGURATION                              #
#                                                                            #
##############################################################################

#' @title
#' Configure the initial state of the square lattice
#'
#' @description
#' Initializes the lattice, setting all its elements to 1
#'
#' @param lattice The lattice that has been initially built
#'
#' @usage
#' config2(lattice)
#'
#' @examples
#' init(50)
#' lattice <- config2(lattice)
#' lattice
#'
#' @export
#'

config2 <- function(Lattice){
  l <- NROW(Lattice)
  return (matrix(rep(1, len=l**2), nrow=l))
}
