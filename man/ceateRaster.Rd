\name{createRaster}
\alias{createRaster}
\alias{createRaster,character-method}

\title{Create an empty Raster}
\usage{
  createRaster(filename,nrow,ncol,extent,nlayers=1,dtype='FLT4S',bandorder='BSQ',crs=NA,overwrite=FALSE,echo=TRUE)
}

\arguments{
  \item{filename}{character specifying the filename (with .grd extension) for the new empty Raster* object}
  \item{nrow}{a numeric value: number of rows}
  \item{ncol}{a numeric value: number of columns}
  \item{extent}{a numeric vector specifying the extent of the new raster including 4 values: xmin, xmax, ymin, and ymax}
  \item{nlayes}{a numeric value: number of raster layers (bands)}
  \item{dtype}{a character (default: 'FLT4S') specifying the type of data for the raster values (see description)}
  \item{bandorder}{a character (default: 'BSQ') specifying the organisation of raster values in the file; can be one of 'BSQ','BIP', 'BIL'}
  \item{crs}{a character specifying definitions for a coordinate reference system}
  \item{overwrite}{logical, if the filename already exists specifying whether the file should be overwritten}
  \item{echo}{logical; whether a message should be printed on the screen to report the success/fail of the new raster file creation}
  }

\description{
This function would be useful specifically when you deal with big raster data. You can create an empty raster file (single or multi-layer) then you can write the raster values partially directly to the file using \code{\link{writeInRaster}} function. Given then the memory mapped to the file, the procedure or reading and writing from/to the file is efficient and quick.

In multi-layer files, the band-order can be set to BIL ('Bands Interleaved by Line'), BIP ('Bands Interleaved by Pixels') or BSQ ('Bands SeQuential').

dtype specifies the data type, determines the interpretation of values written to disk. Datatypes are described by 5 characters. The first three indicate whether the values are integers, decimal number or logical values. The fourth character indicates the number of bytes used to save the values on disk, and the last character indicates whether the numbers are signed (i.e. can be negative and positive values) or not (only zero and positive values allowed).

The following datatypes are available:

\tabular{lll}{
\bold{Datatype definition} \tab \bold{minimum possible value} \tab \bold{maximum possible value} \cr
\code{LOG1S} \tab FALSE (0)\tab TRUE (1) \cr
\code{INT1S} \tab -127 \tab  127 \cr
\code{INT1U} \tab 0 \tab  255 \cr
\code{INT2S} \tab -32,767\tab  32,767 \cr
\code{INT2U} \tab 0 \tab  65,534 \cr
\code{INT4S} \tab -2,147,483,647 \tab 2,147,483,647 \cr
\code{INT4U} \tab 0 \tab 4,294,967,296 \cr
\code{FLT4S} \tab -3.4e+38 \tab  3.4e+38 \cr
\code{FLT8S} \tab -1.7e+308 \tab   1.7e+308 \cr
}

}

\author{Babak Naimi \email{naimi.b@gmail.com}

\url{http://r-gis.net}
}

\examples{
\dontrun{

# creating an empty raster with two layers:
createRaster('test.grd',nrow=10,ncol=10,extent=c(0,10,0,10),nlayers=2)

# reading the empty raster using the brick function from the raster package:
r <- brick('test.grd')

r

r[1:10] # check the values of cells 1:10 from both layers

plot(r) # Obviously, it is empty!


# writing values (randomly generated) into the first band
writeInRaster(rnorm(100),"test.grd",band=1)

plot(r)

# writing values into the second layers:
writeInRaster(1:100,"test.grd",band=2)

plot(r)


r[1:10]
}

}

\keyword{memory,big data, spatial}
