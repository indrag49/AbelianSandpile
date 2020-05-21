##############################################################################
#                                                                            #
#                               STABILIZATION                                #
#                                                                            #
##############################################################################

#' @title
#' Stabilization of a square lattice
#'
#' @description
#' After addition of a sand at a random site of the square lattice, this function checks for any unstabilities that arise. Any toppling site distributes one sand particle to each of its neighbouring site and the process is repeated parallely for any site that is unstable. This process goes on until all the sites are stable and returns the stable lattice, the total number of topplings and the total number of sand particles available in that particular lattice.
#'
#' @param Lattice The configured lattice
#'
#' @usage
#' stabilize(Lattice)
#'
#' @examples
#' init(5)
#' lattice <- config3(lattice)
#' lattice
#' lattice <- add_particle(lattice)
#' lattice
#' lattice <- stabilize(lattice)
#' lattice
#'
#' @export
#'

stabilize <- function(Lattice){
  t <- 0
  L <- NROW(Lattice)
  while (any(Lattice>4)==TRUE){
    if (Lattice[1, 1]>4){
      t<-t+1
      Lattice[1, 2] <- Lattice[1, 2]+1
      Lattice[2, 1] <- Lattice[2, 1]+1
      Lattice[1, 1] <- lattice[1, 1]-4
    }
    if(Lattice[1, L]>4){
      t<-t+1
      Lattice[1, L-1] <- Lattice[1, L-1]+1
      Lattice[2, L] <- Lattice[2, L]+1
      Lattice[1, L] <- Lattice[1, L]-4
    }
    if(Lattice[L, 1]>4){
      t<-t+1
      Lattice[L-1, 1] <- Lattice[L-1, 1]+1
      Lattice[L, 2] <- Lattice[L, 2]+1
      Lattice[L, 1] <- Lattice[L, 1]-4
    }
    if(Lattice[L, L]>4){
      t<-t+1
      Lattice[L-1, L] <- Lattice[L-1, L]+1
      Lattice[L, L-1] <- Lattice[L, L-1]+1
      Lattice[L, L] <- Lattice[L, L]-4
    }
    for(i in 2:L-1){
      if(Lattice[1, i]>4){
        t <- t+1
        Lattice[1, i+1] <- Lattice[1,i+1] +1
        Lattice[1, i-1] <- Lattice[1, i-1]+1
        Lattice[2, i] <- Lattice[2, i]+1
        Lattice[1, i] <- Lattice[1, i]-4
      }
      if(Lattice[L, i]>4){
        t<-t+1
        Lattice[L, i+1] <- Lattice[L,i+1]+1
        Lattice[L, i-1] <- Lattice[L, i-1]+1
        Lattice[L-1, i] <- Lattice[L-1, i]+1
        Lattice[L, i] <- Lattice[L, i]-4
      }
      if(Lattice[i, 1]>4){
        t<-t+1
        Lattice[i-1, 1] <- Lattice[i-1, 1]+1
        Lattice[i+1, 1] <- Lattice[i+1, 1]+1
        Lattice[i, 2] <- Lattice[i, 2]+1
        Lattice[i, 1] <- Lattice[i, 1]-4
      }
      if(Lattice[i, L]>4){
        t<-t+1
        Lattice[i-1, L] <- Lattice[i-1, L]+1
        Lattice[i+1, L] <- Lattice[i+1, L]+1
        Lattice[i, L-1] <- Lattice[i, L-1]+1
        Lattice[i, L] <- Lattice[i, L]-4
      }
      for(j in 2:L-1){
        if(Lattice[i, j]>4){
          t<-t+1
          Lattice[i-1, j] <- Lattice[i-1,j]+1
          Lattice[i+1, j] <- Lattice[i+1, j]+1
          Lattice[i,j+1] <- Lattice[i, j+1]+1
          Lattice[i, j-1] <- Lattice[i, j-1]+1
          Lattice[i, j] <- Lattice[i, j]-4
        }
      }
    }
  }
  return(list(Lattice, t, sum(Lattice)))
}
