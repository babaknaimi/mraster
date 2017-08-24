\name{memory}
\alias{memory}
\alias{memory,characterORmissing-method}
\title{Check the total and free memory in the OS}
\usage{
  memory(u, ...)
}

\arguments{
  \item{u}{a character specifying the memory size unit; can be one of 'B', 'K', 'M' (default), 'G', 'T', for Byte, Kilo-B, Mega-B, Giga-B, and Terra-B respectivey}
  \item{...}{additional arguments (see details)}
  }

\description{
This is a function that checks the total amount of memory on the system, and also the amount that is not used (free memory). This uses the commands from operating system, and would be useful to check an object with what size can be fitted in memory.

As the additional argument, currently the argument \code{add} can be added which is logical (default \code{add=TRUE}) and specifies whether the size unit should be added to the output value (e.g., the memory is represented as "16 Gb" rather than the numeric value 16).
}


\value{
 an numeric or character (if add=T) vector with two items including the size of total and free memory
}

\author{Babak Naimi \email{naimi.b@gmail.com}

\url{http://r-gis.net}


\examples{
memory()

memory(u='G')

memory(u='G', add=F)

}
