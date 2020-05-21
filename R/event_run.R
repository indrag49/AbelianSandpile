##############################################################################
#                                                                            #
#                         THE BTW MODEL SIMULATION                           #
#                                                                            #
##############################################################################

#' @title
#' Main simulation of the Abelian Sandpile (BTW) model
#'
#' @description
#' This function runs the main simulation of the BTW model by taking in a particular lattice state, the total number of sand particles to be added in the simulation and the maximum number of topplings allowed. The function then plots:
#' 1) Average number of sands in the system vs No. of sands added to the system
#' 2) No. of avalanches vs No. of sands added to the system
#' 3) The cumulative probability Prob(s) that an avalanche is of size greater than or equal to s, as a function of s for a lattice of a particular L value
#'
#' @param Lattice A lattice state
#' @param n number of sand particles added to the sytem
#' @param max_topplings number of maximum topplings allowed in the simulation
#'
#' @usage
#' event_run(Lattice, n, max_topplings)
#'
#' @references
#' \url{http://indico.ictp.it/event/8644/material/3/0.pdf}\cr
#' \url{https://en.wikipedia.org/wiki/Abelian_sandpile_model}\cr
#'
#' @examples
#' init(50)
#' lattice <- config3(lattice)
#' event_run(lattice, 2000, 10000)
#'
#' @export
#'

event_run <- function(Lattice, n, max_topplings){
  topplings <- c()
  av_particle <-c()
  for(i in 1:n){
    Lattice <- add_particle(Lattice)
    Z <- stabilize(Lattice)
    Lattice <- Z[[1]]
    topplings <- c(topplings, Z[[2]])
    av_particle <- c(av_particle, Z[[3]])
  }
  prob <- c()
  for(i in 1:max_topplings){
    value <- sum(topplings>=i)/max_topplings
    if (value==0) value <- 10**-6
    prob <- c(prob, value)
  }
  x <- 1:max_topplings
  y <- prob
  plot(1:n, av_particle/NROW(Lattice)**2, type="l", ylim=c(0,5), xlab="No. of sands added to the system", ylab="average number of sands in the system")
  plot(1:n, topplings, type="l", xlab="No. of sands added to the system", ylab="No. of avalanches")
  plot(x, y, type="l", log="xy", ylim=c(10**-5, 1), xlab="s-->", ylab="Prob(s)")
}
