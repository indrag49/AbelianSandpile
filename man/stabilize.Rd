% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stabilize.R
\name{stabilize}
\alias{stabilize}
\title{Stabilization of a square lattice}
\usage{
stabilize(Lattice)
}
\arguments{
\item{Lattice}{The configured lattice}
}
\description{
After addition of a sand at a random site of the square lattice, this function checks for any unstabilities that arise. Any toppling site distributes one sand particle to each of its neighbouring site and the process is repeated parallely for any site that is unstable. This process goes on until all the sites are stable and returns the stable lattice, the total number of topplings and the total number of sand particles available in that particular lattice.
}
\examples{
init(5)
lattice <- config3(lattice)
lattice
lattice <- add_particle(lattice)
lattice
lattice <- stabilize(lattice)
lattice

}
