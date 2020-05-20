##############################################################################
#                                                                            #
#                         INITIALIZATION FUNCTION                            #
#                                                                            #
##############################################################################

#' @title
#' Initialization
#'
#' @description
#' Builds a square lattice, which is a L X L matrix with all elements set to 0
#'
#' @param L an integer value, length of the square lattice
#'
#' @usage
#' init(L)
#'
#' @examples
#' init(70)
#'
#' @importFrom
#' graphics
#' plot
#'
#' @export
#'


init <- function(L){
  lattice <<- matrix(rep(0, len = L**2), nrow= L)
}
