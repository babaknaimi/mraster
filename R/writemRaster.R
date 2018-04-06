#-------------------------------------
# Author: Babak Naimi, naimi.b@gmail.com
# Date (first version): August 2017
# Date (last update):  October 2017
# Version 0.2
# Licence GPL v3
#------------------------



# Extracts  metadata from a Raster* object:
.getRasterMetadata <- function(x) {
  .meta <- list()
  .meta$nrows <- x@nrows
  .meta$ncols <- x@ncols
  .meta$xmin <- x@extent@xmin
  .meta$ymin <- x@extent@ymin
  .meta$xmax <- x@extent@xmax
  .meta$ymax<- x@extent@ymax
  .meta$projection <- x@crs@projargs
  .meta$minValue <- raster::minValue(x, -1, warn=FALSE)
  .meta$maxValue <- raster::maxValue(x, -1, warn=FALSE)
  .meta$nodataValue <- raster:::.nodatavalue(x)
  .meta$z <- x@z
  .meta$history <- x@history

  if (inherits(x,'RasterLayer') || inherits(x,'RasterBrick')) {
    .meta$datanotation <- x@file@datanotation
    .meta$byteorder <- x@file@byteorder
    .meta$bandorder <- x@file@bandorder
    .meta$nlayers <- if (inherits(x,'RasterBrick')) x@data@nlayers else NA
    .meta$isfactor <- x@data@isfactor
    .meta$attributes <- x@data@attributes
    .meta$legend <- x@legend
    .meta$names <- x@data@names
  } else if (inherits(x,'RasterStack')) {
    .meta$nlayers <- length(x@layers)
    .meta$datanotation <- unlist(lapply(1:length(x@layers),function(i) x@layers[[i]]@file@datanotation))
    .meta$byteorder <- unlist(lapply(1:length(x@layers),function(i) x@layers[[i]]@file@byteorder))
    .meta$bandorder <- unlist(lapply(1:length(x@layers),function(i) x@layers[[i]]@file@bandorder))
    .meta$isfactor <- unlist(lapply(1:length(x@layers),function(i) x@layers[[i]]@data@isfactor))
    .meta$names <- unlist(lapply(1:length(x@layers),function(i) x@layers[[i]]@data@names))
    .meta$attributes <- lapply(1:length(x@layers),function(i) x@layers[[i]]@data@attributes)
    .meta$legend <- lapply(1:length(x@layers),function(i) x@layers[[i]]@legend)
  }
  .meta
}
#--------------


# Writes the extracted metadata as a text file (*.grd) following the raster package:
._writeHdrRaster <- function(.meta,filename) {
  # copied from the raster package (Robert Hijmans) and adjusted!
  thefile <- file(filename, "w")  # open an txt file connection
  cat("[general]", "\n", file = thefile, sep='')
  #cat("creator=R package 'sdm' (can be also read by package 'raster')", "\n", file = thefile, sep='')
  cat("creator=R package 'sdm' (can be opened in the 'raster' package as well)", "\n", file = thefile, sep='')
  cat("created=", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", file = thefile, sep='')

  cat("[georeference]", "\n", file = thefile, sep='')
  cat("nrows=",  .meta$nrows, "\n", file = thefile, sep='')
  cat("ncols=",  .meta$ncols, "\n", file = thefile, sep='')
  cat("xmin=", as.character(.meta$xmin), "\n", file = thefile, sep='')
  cat("ymin=", as.character(.meta$ymin), "\n", file = thefile, sep='')
  cat("xmax=", as.character(.meta$xmax), "\n", file = thefile, sep='')
  cat("ymax=", as.character(.meta$ymax), "\n", file = thefile, sep='')
  cat("projection=", .meta$projection, "\n", file = thefile, sep='')

  cat("[data]", "\n", file = thefile, sep='')
  cat("datatype=",  .meta$datanotation[1], "\n", file = thefile, sep='')
  cat("byteorder=", .meta$byteorder[1], "\n", file = thefile, sep='')
  cat("nbands=",  .meta$nlayers, "\n", file = thefile, sep='')
  cat("bandorder=",  .meta$bandorder[1], "\n", file = thefile, sep='')


  # currently only for single layer files!
  if (.meta$nlayers == 1) {
    fact <- .meta$is.factor[1]
    cat("categorical=", paste(fact, collapse=':'), "\n", file = thefile, sep='')
    if (any(fact)) {
      r <- .meta$attributes[[1]]
      cat("ratnames=", paste(colnames(r), collapse=':'), "\n", file = thefile, sep='')
      cat("rattypes=", paste(sapply(r, class), collapse=':'), "\n", file = thefile, sep='')
      cat("ratvalues=", paste(raster::trim(as.character(as.matrix(r))), collapse=':'), "\n", file = thefile, sep='')
    }
    if (length(x@legend@colortable) > 1) {
      cat("colortable=", paste(.meta$legend@colortable, collapse=':'), "\n", file = thefile, sep='')
    }
  }

  cat("minvalue=",  paste(.meta$minValue, collapse=':'), "\n", file = thefile, sep='')
  cat("maxvalue=",  paste(.meta$maxValue, collapse=':'), "\n", file = thefile, sep='')
  cat("nodatavalue=", paste(.meta$nodataValue, collapse=':'), "\n", file = thefile, sep='')

  cat("[legend]", "\n", file = thefile, sep='')
  #cat("legendtype=",  .meta$legend@type, "\n", file = thefile, sep='')
  #cat("values=",  paste(.meta$legend@values, collapse=':'), "\n", file = thefile, sep='')
  #cat("color=",  paste(.meta$legend@color, collapse=':'), "\n", file = thefile, sep='')

  # in .meta generated from RasterStack, these items are list (later should be updated!)
  cat("legendtype=", "\n", file = thefile, sep='')
  cat("values=",  "\n", file = thefile, sep='')
  cat("color=",  "\n", file = thefile, sep='')
  #----------------
  cat("[description]", "\n", file = thefile, sep='')
  ln <- gsub(":", ".", .meta$names)
  cat("layername=", paste(ln, collapse=':'), "\n", file = thefile, sep='')

  if (! is.null(.meta$z) && length(.meta$z) > 0) {
    zname <- names(.meta$z)[1]
    if (is.null(zname)) {
      zname <- 'z-value'
    }
    zclass <- class(z)
    # suggested by Michael Sumner
    if (inherits(.meta$z, "POSIXct")) {
      .meta$z <- format(.meta$z, "%Y-%m-%d %H:%M:%S", tz="UTC")
    } else {
      .meta$z <- as.character(.meta$z)
    }

    cat("zvalues=", paste(c(zname, .meta$z), collapse=':'), "\n", file = thefile, sep='')
    cat("zclass=", zclass, "\n", file = thefile, sep='')
  }

  a <- NULL
  try( a <- unlist(.meta$history), silent=TRUE )
  if (!is.null(a)) {
    cat("history=", a, "\n", file = thefile, sep='')
  }

  a <- NULL
  try( a <- rapply(.meta$history, function(x) paste(as.character(x), collapse='#,#')), silent=TRUE )
  if (!is.null(a)) {
    a <- gsub('\n', '#NL#', a)
    type <- rapply(.meta$history, class)
    type_value <- apply(cbind(type, a), 1, function(x) paste(x, collapse=':'))
    name_type_value <- apply(cbind(names(a), type_value), 1, function(x) paste(x, collapse='='))
    name_type_value <- paste(name_type_value, '\n', sep='')
    cat("[metadata]", "\n", file = thefile, sep='')
    cat(name_type_value, file = thefile, sep='')
  }

  if (any(.meta$isfactor) && length(.meta$attributes) == .meta$nlayers) {
    cat("[categorical]", "\n", file = thefile, sep='')
    cat("factors =",paste0(.meta$names[.meta$isfactor],collapse=':'),'\n',file=thefile,sep='')
    cat("\t[factor levels]\n",file=thefile,sep='')
    r <- .meta$attributes[.meta$isfactor]
    .nl <- .meta$names[.meta$isfactor]
    for (i in 1:length(r)) {
      if (is.data.frame(r[[i]][[1]]) && ncol(r[[i]][[1]]) > 1) {
        .l <- as.character(r[[i]][[1]][,2])
        cat("\t",.nl[i]," =",paste0(.l,collapse=':'),'\n',file=thefile,sep='')
      }
    }
  }
  close(thefile)
  return(TRUE)
}

#------------

# Writes a Raster object as a file (raster native format) to which mraster maps memory:
# (quicker than writeRaster in the raster package)
.writeRaster <- function(x,filename,size=1,datatype='FLT4S',bandorder='BSQ',overwrite=TRUE) {
  .s1Gb <- 1073741824
  .meta <- .getRasterMetadata(x)
  .s <- strsplit(filename,'\\.')[[1]]
  if (length(.s) > 1 && .s[length(.s)] %in% c('grd','gri')) filename <- paste(.s[-length(.s)],collapse = '.')

  if (file.exists(paste0(filename,'.gri'))) {
    if (overwrite) unlink(paste0(filename,'.gri'))
    else stop('filename exists; use different name or use the argument overwrite=TRUE')
  }

  a <- c('FLT8S','FLT4S','INT1U','LOG1S','INT1S','INT2S','INT2U','INT4S','INT4U')
  datatype <- .pmatch(datatype[1],a)
  if (is.na(datatype)) stop('data type (dtype) is not specified appropriately!')
  .meta$datanotation <- datatype
  .meta$bandorder <- bandorder
  dtype <- switch(datatype,'FLT8S'=real64(),'FLT4S'=real32(),'INT1U'=uint8(),'LOG1S'=logi8(),'INT1S'=int8(),
                  'INT2S'=int16(),'INT2U'=uint16(),'INT4S'=int32(),'INT4U'=uint32())
  .mcell <- floor((.s1Gb / sizeof(dtype)) * size)
  .ncell <- (raster::ncell(x) * .meta$nlayers)

  if (inherits(x, 'RasterBrick') || inherits(x, 'RasterLayer')) {
    if (!x@data@inmemory) {
      if (raster::ncell(x) > .mcell) {
        if (bandorder == 'BSQ') x <- raster::writeRaster(x,paste0(filename,'.grd'),datatype=datatype,bandorder='BSQ')
        else x <- raster::writeRaster(x,paste0(filename,'.grd'),datatype=datatype,bandorder='BIP')
      } else {
        zzz <-file(paste0(filename,'.gri'),'ab')
        if (bandorder == 'BSQ') {
          for (j in 1:x@data@nlayers) {
            writeBin(x[[j]][],zzz,size = sizeof(dtype))
          }
          close(zzz)
        } else {
          .x <- c(writeBin(NA, raw(), size=sizeof(dtype)))

          for (j in 1:x@data@nlayers) writeBin(rep(.x, raster::ncell(x)), zzz)

          close(zzz)

          m <- mmap::mmap(paste0(filename,'.gri'),dtype)

          for (j in 1:nlayers(x)) m[seq(j,.ncell,by=x@data@nlayers)]<- x[[j]][]
          munmap(m)

        }


        a <- ._writeHdrRaster(.meta,paste0(filename,'.grd'))
      }
    } else {
      zzz <-file(paste0(filename,'.gri'),'ab')
      if (bandorder == 'BSQ') {
        for (j in 1:x@data@nlayers) {
          writeBin(x[[j]][],zzz,size = sizeof(dtype))
        }
        close(zzz)
      } else {
        .x <- c(writeBin(NA, raw(), size=8))

        for (j in 1:x@data@nlayers) writeBin(rep(.x, raster::ncell(x)), zzz)

        close(zzz)

        m <- mmap::mmap(paste0(filename,'.gri'),dtype)

        for (j in 1:x@data@nlayers) m[seq(j,.ncell,by=x@data@nlayers)]<- x[[j]][]
        munmap(m)
      }

      a <- ._writeHdrRaster(.meta,paste0(filename,'.grd'))
      filename
    }
  } else if (inherits(x,'RasterStack')) {
    if (raster::ncell(x) > .mcell) {
      if (bandorder == 'BSQ') x <- raster::writeRaster(x,paste0(filename,'.grd'),datatype=datatype,bandorder='BSQ')
      else x <- raster::writeRaster(x,paste0(filename,'.grd'),datatype=datatype,bandorder='BIP')
    } else {
      zzz <-file(paste0(filename,'.gri'),'ab')
      if (bandorder == 'BSQ') {
        for (j in 1:.meta$nlayers) {
          writeBin(x[[j]][],zzz,size = sizeof(dtype))
        }
        close(zzz)
      } else {
        .x <- c(writeBin(NA, raw(), size=8))

        for (j in 1:.meta$nlayers) writeBin(rep(.x, raster::ncell(x)), zzz)

        close(zzz)

        m <- mmap::mmap(paste0(filename,'.gri'),dtype)

        for (j in 1:.meta$nlayers) m[seq(j,.ncell,by=.meta$nlayers)]<- x[[j]][]
        munmap(m)
      }

      a <- ._writeHdrRaster(.meta,paste0(filename,'.grd'))
      filename
    }
  }

}

#-----------------


.getGDALmeta <- function(filename) {
  filename <- .normalizePath(filename)
  .inf <- rgdal::GDALinfo(filename)
  .attr <- attributes(.inf)
  .meta <- list()
  .meta$nrows <- .inf[[1]]
  .meta$ncols <- .inf[[2]]
  .meta$xmin <- .inf[[4]]
  .meta$ymin <- .inf[[5]]
  .meta$xmax <- .inf[[4]] + (.inf[[2]]*.inf[[6]])
  .meta$ymax<- .inf[[5]] + (.inf[[1]]*.inf[[7]])
  .meta$projection <- .attr$projection
  ### based on the first layer (to be checked for rasters with different data.type:
  .meta$datanotation <- switch(as.character(.attr$df[1,1]),'Float64'='FLT8S','Float32'='FLT4S','Byte'='INT1U', 'UInt16'='INT2U', 'Int16'='INT2S', 'UInt32'='INT4U','Int32'='INT4S','CInt16'='INT2S', 'CInt32'='INT4S', 'CFloat32'='FLT4S', 'CFloat64'='FLT8S')
  .meta$byteorder <- "little"
  .meta$bandorder <- 'BSQ'
  .meta$nlayers <- .inf[[3]]
  .meta$isfactor <- NULL
  .meta$attributes <- list()
  .meta$legend <- new(".RasterLegend")
  .meta$minValue <- NULL
  .meta$maxValue <- NULL
  .meta$nodataValue <- .attr$df[1,3]
  .meta$names <- NULL
  .meta$z <- list()
  .meta$history <- list()
  .meta
}
#--------------

.getGDALband <- function(filename,band=1) {
  xx <- new("GDALReadOnlyDataset", filename)
  .xx <- as.vector(rgdal::getRasterData(xx,band = band))
  rgdal::GDAL.close(xx)
  .xx
}

#----------- check whether the driver for a raster dataset is gdal:
.isGDAL <- function(x) {
  if (inherits(x,'Raster')) {
    if (inMemory(x)) return(FALSE)
    else return(x@file@driver == 'gdal')
  } else if (inherits(x,'character')) {
    if (!file.exists(x)) stop('filename does not exist!')
    .meta <- try(.getGDALmeta(x),silent = TRUE)
    if (inherits(.meta,"try-error")) return(FALSE)
    else return(TRUE)
  }
}
#---------
# get driver for a raster dataset!
.getDriver <- function(x) {
  if (inherits(x,'Raster')) {
    if (inMemory(x)) return('memory')
    else return(x@file@driver)
  } else if (inherits(x,'character')) {
    if (!file.exists(x)) stop('filename does not exist!')
    return(brick(x)@file@driver)
  }
}


#---------------
.writeGDALRaster <- function(filename,newfile,layerNames=NULL,datatype=NULL,overwrite=TRUE) {
  if (!file.exists(filename)) stop('filename does not exist!')
  .meta <- try(.getGDALmeta(filename),silent = TRUE)
  if (inherits(.meta,"try-error")) stop('file format is not recognised by rgdal!')

  .s <- strsplit(newfile,'\\.')[[1]]
  if (length(.s) > 1 && .s[length(.s)] %in% c('grd','gri')) newfile <- paste(.s[-length(.s)],collapse = '.')


  if (file.exists(paste0(newfile,'.gri'))) {
    if (overwrite) unlink(paste0(newfile,'.gri'))
    else stop('newfile exists; use different name or use the argument overwrite=TRUE')
  }

  xx <- new("GDALReadOnlyDataset", filename)

  .min <- .max <- rep(NA,.meta$nlayers)

  if (!is.null(datatype)) {
    a <- c('FLT8S','FLT4S','INT1U','LOG1S','INT1S','INT2S','INT2U','INT4S','INT4U')
    datatype <- sdm:::.pmatch(datatype[1],a)
    if (!is.na(datatype)) .meta$datanotation <- datatype
  }

  dtype <- switch(.meta$datanotation,'FLT8S'=real64(),'FLT4S'=real32(),'INT1U'=uint8(),'LOG1S'=logi8(),'INT1S'=int8(),
                  'INT2S'=int16(),'INT2U'=uint16(),'INT4S'=int32(),'INT4U'=uint32())
  zz <- file(paste0(newfile,'.gri'),'ab')
  for (i in 1:.meta$nlayers) {
    .xx <- as.vector(rgdal::getRasterData(xx,band = i))
    .min[i] <- min(.xx,na.rm = TRUE)
    .max[i] <- max(.xx,na.rm = TRUE)
    writeBin(.xx,zz,size = sizeof(dtype))
  }
  close(zz)
  rgdal::GDAL.close(xx)
  .meta$maxValue <- max(.max)
  .meta$minValue <- min(.min)
  if (!is.null(layerNames) && length(layerNames) == .meta$nlayers) .meta$names <- layerNames
  else .meta$names <- paste0('band',1:.meta$nlayers)

  ._writeHdrRaster(.meta,paste0(newfile,'.grd'))
}
#-------------------------


if (!isGeneric("writemRaster")) {
  setGeneric("writemRaster", function(x,filename,...)
    standardGeneric("writemRaster"))
}



setMethod('writemRaster', signature(x='Raster'),
          function(x,filename,datatype,bandorder,overwrite,size,...) {
            if (missing(filename)) stop('filename is missed!')
            if (missing(bandorder)) bandorder <- 'BSQ'
            if (!require(mmap)) stop('package mmap is required, but is not installed!')
            if (!bandorder %in% c('BSQ','BIP')) {
              bandorder <- 'BSQ'
              warning('bandorder should be either BSQ or BIP; it is changed to the default value, i.e. BSQ')
            }

            if (missing(size)) size <- 0.9 * memory('G',echo=FALSE,.r=NULL)[2]
            #------
            #.writeRaster


          }
)
