##############################################################################
#                                                                            #
#                         LATTICE CONFIGURATION                              #
#                                                                            #
##############################################################################

#' @title
#' Configure the initial state of the square lattice
#'
#' @description
#' Initializes the lattice, setting all its elements to be the critical value, 4
#'
#' @param lattice The lattice that has been initially built
#'
#' @usage
#' config4(lattice)
#'
#' @examples
#' init(50)
#' lattice <- config4(lattice)
#' lattice
#'
#' @export
#'

config4 <- function(Lattice){
  l <- NROW(Lattice)
  return (matrix(rep(4, len=l**2), nrow=l))
}
