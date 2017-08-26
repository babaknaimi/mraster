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

The additional arguments:

\code{echo}: logical (default \code{echo=TRUE}), specifies whether the output (with size unit) should be printed on screen (e.g., "16 Gb").

\code{session}: logical (default \code{session=FALSE}), if TRUE, the memory used by the current session is also reported.

\code{.r}: numeric (default \code{.r=5}), the decimal degrees to which the output values are rounded.
}


\value{
 a numeric vector with two items including the size of total and free memory. If session=T, an additional item (used_by_this_session) is included as well.
}

\author{Babak Naimi \email{naimi.b@gmail.com}

\url{http://r-gis.net}
}

\examples{
memory()

m <- memory(u='G') # by default, the report is printed on screen, but a numeric vector is returned

m

m <- memory(u='G', echo=FALSE) # the report is not printed on screen

m

memory(session=TRUE) # the size of memory used by the current session is also included

# unit in Tb (Terra byte):
memory(u='T',session=TRUE, .r=4) # the values are rounded to 4 decimals.

memory(u='G',session=TRUE, .r=NULL) # the values are returned without any round!

}

\keyword{memory,big data, spatial}
