\name{writeInRaster}
\alias{writeInRaster}
\alias{writeInRaster,numeric-method}
\alias{writeInRaster,matrixORdata.frame-method}

\title{Write values in an existing raster}
\usage{
  writeInRaster(x,filename,band,cells)
}

\arguments{
  \item{x}{a numeric vector, matrix, or data.frame containing values for one (when x is a vector) or several raster layers (bands)}
  \item{filename}{character specifying the filename (with .grd extension) for the Raster* object}
  \item{band}{specifying the bands (layers) to which the values should be written}
  \item{cells}{a numeric vector specifying the cell number in the Raster* file to which the values from x will be written}
  }

\description{
Using this function, writing the values into an existing Raster file would be efficient as the file of the Raster object is not loaded into memory, but it is mapped to memory and the values in x will be written directly to the corresponding cells within the file.

It works using the native Raster format (.grd) used by the package 'raster', that is also used as the main format by the mraster package.

When you get access to a raster file though using \link[mraster]{mraster}, you can also use writeValues function as part of the \code{mmapRaster} R6 object (see example).
}


\author{Babak Naimi \email{naimi.b@gmail.com}

\url{http://r-gis.net}
}

\examples{
\dontrun{

file <- system.file("external/bioclim.grd", package="mraster")

file # name of the file


r <- brick(file) # using the brick file (from the raster package) to read the file!

r  # it has 3 layers

plot(r)

v <- r[111:120] # values of the 3 layers from 10 cells 111:120

v # values of the 3 layers from 10 cells 111:120

# To write the values 501:510 to the cells 111:120 in the first raster layer:
writeInRaster(x=c(501:510),filename=file,band = 1,cells=111:120)


r[111:120] # you can see that the values of the first band in the raster file is changed!

# To write the values 501:505 to the cells 111:115 in the first and third raster layers:
writeInRaster(x=data.frame(c(1001:1005),c(2001:2005)),filename=file,band = c(1,3),cells=111:115)


r[111:120] # see the changes!

plot(r) # in the visualisation, the changes of the cells are clear!

# let's assign the original values of those cells from the matrix v

writeInRaster(x=v,filename=file,band = 1:3,cells=111:120)

r[111:120]

plot(r)

###################
###################

# It is also possible to write the values when the Raster object is read as a mmapRaster object:

m <- mraster(file)

m


m$getValues(111:120)

m$writeValues(x=c(501:510),band = 1,cells=111:120)

m$getValues(111:120)

m$writeValues(x=v,cells=111:120) # write original values back

m$getValues(111:120)
}

}

\keyword{mmap, big data, spatial}
